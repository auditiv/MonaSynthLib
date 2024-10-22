{-# LANGUAGE ScopedTypeVariables #-}
module MonaSynthLib.Notes where

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


------------------------------Notes Map----------------------------------------------


type Note = String

myNote :: Note
myNote = "A4"

constantExp :: NiceValue.T a -> Exp a
constantExp val = Expr.Exp $ pure val


rat2ExpFloat :: Rational -> Exp Float
rat2ExpFloat a = constantExp (NiceValue.fromRational' a)

int2ExpIn :: Integer -> Exp Word
int2ExpIn a = constantExp (NiceValue.fromInteger' a)

--constantExp (NiceValue.fromInteger' 1000)
{-  ghci> t =  constantExp (NiceValue.fromInteger' 1000)
    ghci> playMono (Causal.take t $* (monoGen Saw) (lookup' (notePeriods frequencies) c))
    ghci>  -}

-- Frequencies (in Hz) from your image (columns representing octaves 4 and 5)
frequencies :: [(Note, Rational)] --3 octaves
frequencies = [ ("P",  1 :: Rational), ("C3",  13.081 :: Rational), ("C#3",  13.859 :: Rational), ("D3",   14.683 :: Rational), ("D#3",  15.556 :: Rational),
                ("E3",  164.81 :: Rational), ("F3",  174.61 :: Rational), ("F#3",  184.99 :: Rational), ("G3",  196.00 :: Rational),
                ("G#3", 207.65 :: Rational), ("A3",  220.00 :: Rational), ("A#3",  233.08 :: Rational), ("B3",  246.94 :: Rational),
                ("C4",  261.63 :: Rational), ("C#4", 277.18 :: Rational), ("D4",   293.66 :: Rational), ("D#4", 311.13 :: Rational ),
                ("E4",  329.63 :: Rational), ("F4",  349.23 :: Rational), ("F#4",  369.99 :: Rational), ("G4",  392.00 :: Rational  ),
                ("G#4", 415.30 :: Rational), ("A4",  440.00 :: Rational), ("A#4",  466.16 :: Rational), ("B4",  493.88 :: Rational  ),
                ("C5",  523.25 :: Rational), ("C#5", 554.37 :: Rational), ("D5",   587.33 :: Rational), ("D#5", 622.25 :: Rational ),
                ("E5",  659.25 :: Rational), ("F5",  698.46 :: Rational), ("F#5",  739.99 :: Rational), ("G5",  783.99 :: Rational ),
                ("G#5", 830.61 :: Rational), ("A5",  880.00 :: Rational), ("A#5",  932.33 :: Rational), ("B5",  987.77 :: Rational ) ] 

-- Function to calculate period length for a given frequency
periodLength :: (Rational) -> Rational
periodLength freq = (/) freq 44100

-- Creating a Map from note names to their respective period lengths
notePeriods :: Map.Map Note (Rational)
notePeriods  = Map.fromList $ fmap (\(note, freq) -> (note, periodLength freq)) frequencies

-- implements the lookup function, but for reactive prog name play fits better 
play :: Note -> Exp Float
play key = do
    let res = Map.lookup key notePeriods
    case res of
            Just freq -> (rat2ExpFloat freq)
            Nothing -> rat2ExpFloat(1%100)-- play 441 Hz 


{-
ghci> d = "D#4" :: Note
ghci> playMono ( Causal.take 22050 $* ((monoGen Sine) (play d)))
 -}
c,d,dis::Note
c = "C4"
d = "D4"
dis = "D#4"
