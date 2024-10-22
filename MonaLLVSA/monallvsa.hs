{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where
  
import MonaSynthLib

--import Control.Concurrent (forkIO, threadDelay,readMVar,newMVar, MVar, newEmptyMVar,modifyMVar_, putMVar, tryTakeMVar)

import Control.Concurrent.Async (async, wait, cancel, Async)
import Control.Concurrent (threadDelay, forkIO, Chan, newChan, writeChan, readChan, dupChan, MVar,newMVar, readMVar, modifyMVar_,newEmptyMVar, putMVar, takeMVar, forkIO, killThread, ThreadId)

import Control.Monad ( forever, unless, when, join, unless, when )
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Exit (exitSuccess) -- Import exitSuccess to quit the program
--import Synthesizer.LLVM.ExampleUtility no neccessary just for asStereot which is just idimportimport
import qualified Synthesizer.LLVM.Filter.ComplexFirstOrderPacked as BandPass
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
--import qualified Synthesizer.LLVM.Filter.Butterworth as Butterworth
import qualified Synthesizer.LLVM.Causal.ControlledPacked as CtrlPS
import qualified Synthesizer.LLVM.Causal.Controlled as Ctrl
import qualified Synthesizer.LLVM.Causal.Render as CausalRender

import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Causal.Functional as Func
import qualified Synthesizer.LLVM.Generator.SignalPacked as SigPS
import qualified Synthesizer.LLVM.Generator.Core as SigCore
import qualified Synthesizer.LLVM.Generator.Source as Source
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import qualified Synthesizer.LLVM.Interpolation as Interpolation
import qualified Synthesizer.LLVM.Storable.Signal as SigStL
import qualified Synthesizer.LLVM.ConstantPiece as Const
import qualified Synthesizer.LLVM.Wave as Wave
import Synthesizer.LLVM.Causal.Functional (($&), (&|&))
import Synthesizer.LLVM.Causal.Process (($<), ($>), ($*), ($<#), ($*#))

import qualified Synthesizer.LLVM.Frame.StereoInterleaved as StereoInt
import qualified Synthesizer.LLVM.Frame.Stereo as Stereo
import qualified Synthesizer.LLVM.Frame.SerialVector as Serial

import qualified LLVM.DSL.Expression.Maybe as ExprMaybe
import qualified LLVM.DSL.Expression as Expr
import LLVM.DSL.Expression (Exp, (>*), (&&*))

import qualified LLVM.Extra.Nice.Value.Marshal as Marshal
import qualified LLVM.Extra.Nice.Vector.Instance as NiceVectorI
import qualified LLVM.Extra.Nice.Vector as NiceVector
import qualified LLVM.Extra.Nice.Value as NiceValue
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Tuple as Tuple
import qualified LLVM.Extra.Maybe as Maybe

import qualified LLVM.Core as LLVM
import LLVM.Util.Arithmetic () -- Floating instance for TValue

import qualified Type.Data.Num.Decimal as TypeNum
import Type.Data.Num.Decimal (D2, D4, (:*:))
import Type.Base.Proxy (Proxy)

import qualified Synthesizer.CausalIO.Process as PIO
import qualified Synthesizer.Causal.Class as CausalClass
import qualified Synthesizer.Zip as Zip
import qualified Synthesizer.State.Control as CtrlS
import qualified Synthesizer.State.Signal as SigS

import qualified Synthesizer.Plain.Filter.Recursive as FiltR
import qualified Synthesizer.Plain.Filter.Recursive.FirstOrder as Filt1Core

import Control.Arrow (Arrow, arr, first, (&&&), (^<<), (<<^), (***))
import Control.Category ((<<<), (.), id)

import Control.Applicative (liftA2)

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import Data.Function.HT (nest)
import Data.NonEmpty ((!:))
import Data.Semigroup ((<>))

import Data.Traversable (sequenceA)
import Data.Tuple.HT (mapSnd)
import System.Path ((</>))
import System.Random (randomRs, mkStdGen)

import qualified System.Unsafe as Unsafe
import qualified System.IO as IO
import Control.Exception (bracket)

import qualified Algebra.Field as Field

import qualified NumericPrelude.Numeric as NP
import qualified Prelude as P
import NumericPrelude.Numeric (fromIntegral, sum, (+), (-), (/), (*))
import Prelude hiding (fst, id, (.), fromIntegral, sum, (+), (-), (/), (*))
--Multithreading Imports  
import Control.Concurrent.Async
import System.IO
import System.Exit (exitSuccess)
import System.Console.Haskeline -- for more advanced input handling
-- simples of all things:
import Type.Data.Num.Decimal (D4, D8, d8)
import qualified Algebra.Ring as Ring
import qualified Synthesizer.Generic.Control as Wave
import qualified Control.Applicative as Wave
import qualified Number.Complex as A
import qualified LLVM.Extra.Arithmetic as A
import qualified LLVM.Extra.Arithmetic as A
import Data.Map as Map
import Data.IORef

import System.IO.Unsafe (unsafePerformIO)
{- StackExChange:
    The unsafePerformIO function can be used in any situation where,
    You know that its use is safe, and
    You are unable to prove its safety using the Haskell type system.
    ~   https://stackoverflow.com/questions/10529284/is-there-ever-a-good-reason-to-use-unsafeperformio
 -}



dualOsciStereo :: Waveshape -> Exp Float -> Sig.T (Stereo.T (NiceValue.T Float))
dualOsciStereo Sine freq =
   liftA2 Stereo.cons
      (Sig.osci Wave.sine 0.0 (freq*1.001) +
       Sig.osci Wave.sine 0.2 (freq*1.003))
      (Sig.osci Wave.sine 0.1 (freq*1.005) +
       Sig.osci Wave.sine 0.5 (freq*0.997))

dualOsciStereo Saw freq =
   liftA2 Stereo.cons
      (Sig.osci Wave.saw 0.0 (freq*1.001) +
       Sig.osci Wave.saw 0.2 (freq*1.003))
      (Sig.osci Wave.saw 0.1 (freq*1.004) + 
       Sig.osci Wave.saw 0.5 (freq*0.998))


-------------------------------- THREAD MAP -------------------------------------------------------------------------

type GenId = Int



type ThreadMap = Map.Map GenId ThreadId
-- maps each Generator that produces sound to a threadId 
threadmap :: Map.Map GenId ThreadId --3 octaves
threadmap = Map.fromList []

-- Global mutable reference for the session
{-# NOINLINE session #-}
session :: IORef (Map GenId ThreadId)
session = unsafePerformIO (newIORef threadmap)

alreadyExist :: GenId -> IO(Bool)
alreadyExist genId = do
  tmap <- readIORef session  -- Read the current session 
  if (Map.member genId tmap) then do
    return True
    else do
      return False



addGenToMap :: GenId -> ThreadId -> IO (Bool)
addGenToMap key tid = do
  tmap <- readIORef session  -- Read the current session 
  if (Map.member key tmap)
    then do
      putStrLn "ID already exists"
      return False       -- did not succeed in adding    
    else do
      putStrLn "Adding new thread ID."      
      let newMap = Map.insert key tid tmap   
      writeIORef session newMap  -- Write the updated map back to session
      return True        -- successfull added

-- in cabal repl: forkIO (playMono $ monoGenSaw (lookup' (notePeriods frequencies) c)) >> return () tid discarded

forking :: GenId -> IO () -> IO () --returns threadid
forking genId action = do
    exists <- alreadyExist genId
    if exists then do
      putStrLn "Try a new Key, this one already exists"
      else do
         tid <- forkIO action  -- Fork new thread and get its ThreadId
         res <- addGenToMap genId tid  -- Try to add ThreadId to the session map
         case res of
           True -> putStrLn $ "Forked thread with GenId " ++ show genId
           False -> putStrLn "Failed to add GenId. It may already exist."

-- killThread tid  -- This stops the oscillator

tupdate :: GenId -> IO () ->IO ()
tupdate genId action = do
    tmap <- readIORef session
    let res = Map.lookup genId tmap
    case res of
        Just tid -> do 
            putStrLn $ "Killing thread with ID: " ++ show tid
            killThread tid  -- Kill the existing thread
            putStrLn $ "Restarting action for GenId: " ++ show genId
            forking genId action  -- Restart the action
        Nothing -> putStrLn "GenId not found in session."
   
kill :: GenId -> IO ()
kill genId = do
  tmap <- readIORef session
  let res = Map.lookup genId tmap
  case res of
        Just tid -> do 
            putStrLn $ "Killing thread with ID: " ++ show tid
            killThread tid  -- Kill the existing thread            
        Nothing -> putStrLn "GenId not found in session."

killAll ::  IO()
killAll = do
  tmap <- readIORef session
  let listOfThreads = Map.elems tmap in
    mapM_ (killThread) listOfThreads  -- Apply `kill` to each ThreadId in the list (Monadic map fct)


showSess :: IO()
showSess = do
  tmap <- readIORef session
  let listOfAll = Map.toList tmap
  print listOfAll

{- ghci> t <- forking $ playMono $ monoGenSine $ lookup' notePeriods dis
ghci> killThread t -}
-- create Preset Thread Table, init is empty

main :: IO ()
main = do
  -- for checking that Dockercontainer runs correctly:
  putStrLn "For starting MonaLLVsa enter with 'cabal v2-repl monallvsa' command ..."



{-
 
+-----1a----+          +---1b----------------2---+            +----3----+
| Generator |>>=chIn>>=|envFlter-----|-----Filter|>>=chOut=>> |Mvar Out | 
+-----------+     |    +-------------------------+     |      +---------+ 
               DRAINER                             DRAINER
-}

  -- Example Main function to run MonaLLySA (Monadic LLVM y Synthesizer Application)
mainVectorized :: IO ()
mainVectorized = do
    hSetBuffering stdout NoBuffering  -- Disable output buffering for real-time output

    -- create MVar (shared variable / semaphor)
    mvarState <- newEmptyMVar
    putMVar mvarState (1) --playing

    -- Create a channels for communication between blocks/threads
    chanIn <- newChan
    chanOut<- newChan

    -- Start generator, envelope attack, and DSP filter in separate threads
    genTask <- async $ monoGenerator (monoGen Sine) 0.02 chanIn

    filterTask <- async $ filterEnv2LazyVecMono mvarState causalMonoEnvAtack (simpleMonoHPF 0.5) chanIn chanOut

    -- Output task that sends modulated signal to boxes

    outTask <- async $ lazyVectorOutputMono chanOut

    threadDelay 1500000  -- Processing time (1.0s)
    putStrLn "11.0s passed: you played all samples and MidChannel should be empty"
    putMVar mvarState (1)

    -- end all tasks and free memory
    cancel outTask
    cancel genTask   
    cancel filterTask 
    -- free channel memory
    drainTask1 <- async $ drainChanMono chanIn    
    cancel drainTask1     