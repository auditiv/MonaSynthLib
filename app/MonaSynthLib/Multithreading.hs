
module MonaSynthLib.Multithreading where

import MonaSynthLib.Generators
import MonaSynthLib.Notes
import MonaSynthLib.Play
import MonaSynthLib.Filters
import MonaSynthLib.Mixers

import qualified Data.StorableVector.Lazy as SVL

import Control.Concurrent.Async (async, wait, cancel, Async)
import Control.Concurrent (threadDelay, forkIO, Chan, newChan, writeChan, readChan, dupChan, MVar,newMVar, readMVar, modifyMVar_,newEmptyMVar, putMVar, takeMVar, forkIO, killThread, ThreadId)
--import Control.Concurrent (forkIO, threadDelay,readMVar,newMVar, MVar, newEmptyMVar,modifyMVar_, putMVar, tryTakeMVar)
import Control.Monad (forever, unless)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Exit (exitSuccess) -- Import exitSuccess to quit the program
--import Synthesizer.LLVM.ExampleUtility no neccessary just for asStereot which is just idimportimport
import qualified Synthesizer.LLVM.Filter.ComplexFirstOrderPacked as BandPass
import qualified Synthesizer.LLVM.Filter.Allpass as Allpass
import qualified Synthesizer.LLVM.Filter.Butterworth as Butterworth
import qualified Synthesizer.LLVM.Filter.Chebyshev as Chebyshev
import qualified Synthesizer.LLVM.Filter.FirstOrder as Filter
import qualified Synthesizer.LLVM.Filter.SecondOrder as Filt2
import qualified Synthesizer.LLVM.Filter.SecondOrderPacked as Filt2P
import qualified Synthesizer.LLVM.Filter.Moog as Moog
import qualified Synthesizer.LLVM.Filter.Universal as UniFilter
import qualified Synthesizer.LLVM.Filter.NonRecursive as FiltNR
import qualified Synthesizer.LLVM.Causal.Helix as Helix
import qualified Synthesizer.LLVM.Causal.ControlledPacked as CtrlPS
import qualified Synthesizer.LLVM.Causal.Controlled as Ctrl
import qualified Synthesizer.LLVM.Causal.Render as CausalRender

import qualified Synthesizer.LLVM.Causal.ProcessPacked as CausalPS
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Causal.Functional as Func
import qualified Synthesizer.LLVM.Generator.Render as Render
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
import Control.Functor.HT (void)
import Control.Monad (when, join)

import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import Foreign.Storable (Storable)

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Numeric.NonNegative.Wrapper as NonNeg

import qualified Sound.Sox.Option.Format as SoxOption
import qualified Sound.Sox.Play as SoxPlay
-- import qualified Synthesizer.ALSA.Storable.Play as Play

import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import Data.Function.HT (nest)
import Data.NonEmpty ((!:))
import Data.Semigroup ((<>))
import Data.Map as Map

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
import Control.Monad (unless, when)
import System.Console.Haskeline -- for more advanced input handling
-- simples of all things:
import Type.Data.Num.Decimal (D4, D8, d8)
import qualified Algebra.Ring as Ring
import qualified Synthesizer.Generic.Control as Wave
import Data.Ratio
--for writing .wav-Files
import Codec.Audio.Wave as Wav
import GHC.Word (Word32)
----------------------------------------- MULTITHREAD :-----------------------------------------------------------

{- Implementing MonaLLVsa MONO style with 
   attack envelop and a limited the buffersize

   Return a lazy list representing the contents of the supplied Chan, much like hGetContents.

   Generator: continuously generates numbers and writes them to channel In
   Channel 1: for the Atack and the Sustain part of the Envelope

   vectorizeSignal :: Int -> Sig.T (NiceValue.T Float) -> SVL.Vector (Float) -}
    

monoVectorGenerator :: Chan (SVL.Vector Float) -> IO ()
monoVectorGenerator chanIn = do
    writeChan chanIn  . asMono . SVL.take 10000 =<< helperMonoSig2Vec 40000 (monoGen Sine 0.0001)
    putStrLn "Just sent 10000 samples"
    
monoGenerator :: (Exp Float ->  Sig.MV Float) -> Exp Float -> Chan(Sig.MV Float) -> IO ()
monoGenerator gen freq chanOut = do
   writeChan chanOut (gen freq)

monoChordGenerator :: Chan (Sig.MV Float) -> IO ()
monoChordGenerator chanOut = do
   writeChan chanOut (mixChord Sine [0.01, 0,002])
     

   
-- Filter: reads signal from the channel and apply processing defined in the given function
filterEnv2LazyVecMono :: MVar Int-> (Sig.MV Float -> Sig.MV Float) -> 
                           (Sig.MV Float -> Sig.MV Float) -> Chan (Sig.MV Float) -> 
                             Chan (SVL.Vector Float) ->  IO ()
filterEnv2LazyVecMono state attackenv fx chanIn chanOut  = do
   let loop = do

        currState <- takeMVar state -- read current state
        signal <- readChan chanIn  -- read channel
       
        putStrLn ("current MVar State is:" ++ show currState)
        sigChunk <- helperMonoSig2Vec 88200 (attackenv (fx signal))  -- Process signal with fx and make chunk
        writeChan chanOut . asMono . SVL.take 88200 $ sigChunk
   putStrLn "DSP Filter put 2X44100 samples into pipeline!"  -- Adjust message to reflect the size
   loop


-- Output: Send Signal to the Speakers    
mixerOutputMono :: Float -> Float -> Float -> Chan (SVL.Vector Float)-> Chan (SVL.Vector Float) -> IO ()
mixerOutputMono volMaster vol1 vol2 channel1 channel2 = forever $ do
   
   -- take signal from channel
   signal1 <- readChan channel1
   signal2 <- readChan channel2
   -- watch out for the clipping when mixing
   let a = mixVectorHaskell (applyVolumeHaskell vol1 signal1) (applyVolumeHaskell vol2  signal2)
   playMonoVector $ applyVolumeHaskell volMaster a

lazyVectorOutputMono ::  Chan (SVL.Vector Float) -> IO ()
lazyVectorOutputMono channel1 = do
   -- Define the recursive loop
   let loop = do
         signal1 <- readChan channel1
         playMonoVector $ signal1
         --putStrLn "I've sent 44100 samples to output"
         loop  -- Recursive call to continue the loop
   loop  -- Start the loop


-- Drainer : Read channels empty (so they can be freed!)
drainChanMono :: Chan (Sig.MV Float) -> IO ()
drainChanMono chan1 = do
    let loop = do
          _ <- readChan chan1  -- discard the value (by reading it)
          loop  -- Continue draining
    loop
    -- recursive draining

writeChanToFileMono :: Chan (Sig.MV Float) -> IO ()
writeChanToFileMono channel = do
    -- Create a duplicate of the channel to allow independent reading
    chanLocal <- dupChan channel
    signal <- readChan chanLocal
    
    writeSignalToFileMono "test.f32" signal
    drainChanMono chanLocal

---------------------------------------------Input From User ------------------------------------------------------
userInput :: Async () -> IO ()
userInput asyncTask = do
    input <- getChar
    if input == 'Q'
        then do
            putStrLn "Quitting..."
            cancel asyncTask -- Cancel the async task if it's still running
            -- print final count:
            exitSuccess      -- Exit the program
        else userInput asyncTask-- Keep waiting for input

------------------------- experimental -------------------------------------------------------------------------------

updateSound :: Async () -> IO ()
updateSound asyncTask = do
    input <- getChar  -- Reads a single character from the user input
    case input of                          
        
        'Q' -> do
            putStrLn "Quitting..."
            cancel asyncTask  -- Cancel the async task if it's still running
            exitSuccess       -- Exit the program

        _ -> do
            putStrLn "Unknown command. Press 'Q' to quit."
            userInput asyncTask  -- Loop again to wait for more input



---------------------------------------------------------------------