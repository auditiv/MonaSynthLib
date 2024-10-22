module MonaSynthLib.Generators where

import MonaSynthLib.Notes
import MonaSynthLib.Filters

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

-------------------------------Simple GENERATORS -------------------------------------------------------


data Waveshape = Saw | Sine | Tri | Square | Trapz


monoGen :: Waveshape -> Exp Float -> Sig.MV Float
monoGen Saw    freq = Sig.osci Wave.saw 0.0 freq
monoGen Sine   freq = Sig.osci Wave.sine 0.0 freq
monoGen Tri    freq = Sig.osci Wave.triangle 0.0 freq
monoGen Square freq = Sig.osci Wave.square 0.0 freq
monoGen Trapz  freq = Sig.osci (Wave.trapezoidSlope (A.fromRational' 5) ) 3.0 freq

--- create and mix multiple Generators into one single mono signal

mixSynth :: [Waveshape] -> [Exp Float] -> Sig.MV Float
mixSynth waves notes =
    case (createNonEmptyListFromList waves, createNonEmptyListFromList notes) of
        (Just nonEwaves , Just nonEnotes) -> 
            NonEmpty.foldBalanced (+) $ NonEmpty.zipWith monoGen nonEwaves nonEnotes
        _ -> error "Both lists must be non-empty"



---------------------------------- CHORDS ----------------------------------------------------------------
mixChord :: Waveshape -> [Exp Float] -> Sig.MV Float
mixChord wave notes =
    case createNonEmptyListFromList notes of
        Nothing -> error "List of notes cannot be empty!"
        Just nonEmptyNotes -> NonEmpty.foldBalanced (+) $ fmap (\tone -> monoGen wave tone) nonEmptyNotes

 
monoChordBluesSequence :: Sig.MV Float
monoChordBluesSequence =
   NonEmpty.foldl1(<>) $ -- important to fold to the leeft here!
   fmap (\tone -> Causal.take 52300 $*(Causal.envelope 
   $< (Sig.exponential2 11150 1) $*(Causal.envelope 
   $< (1 - Sig.exponential2 5000 0) $*  ((monoGen Saw) (tone))))) $
   fmap (\note -> play note) $
   ("G5" :: Note) !:("C4" :: Note) : ("F4:q" :: Note) : ("P" :: Note)
    :("B4" :: Note) : ("C4" :: Note) : ("B3" :: Note) : ("P" :: Note)
    :("G3" :: Note) : ("C3" :: Note) : ("P" :: Note)  :("C3" :: Note)
    : ("P" :: Note)  : []

------------------------------- Stereo Generator -----------------------------------------------------------
stereoGenSaw :: Exp Float -> Sig.T (Stereo.T (NiceValue.T Float))
stereoGenSaw freq =
   liftA2 Stereo.cons
      (Sig.osci Wave.saw 0.0 (freq*1.001) +
       Sig.osci Wave.saw 0.2 (freq*1.003) +
       Sig.osci Wave.saw 0.1 (freq*0.995))
      (Sig.osci Wave.saw 0.1 (freq*1.005) +
       Sig.osci Wave.saw 0.7 (freq*0.997) +
       Sig.osci Wave.saw 0.5 (freq*0.999))

------ Helper functions ---------------------------

-- Create a NonEmpty list from a regular list
createNonEmptyListFromList :: [a] -> Maybe (NonEmpty.T [] (a))
createNonEmptyListFromList []     = Nothing
createNonEmptyListFromList (x:xs) = Just (x !: xs)

