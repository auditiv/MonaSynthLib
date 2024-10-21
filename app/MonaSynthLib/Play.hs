module MonaSynthLib.Play where

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
------------------- Play Signals ---------------------------------------------------
{-You need to have SoX installed to run these commands -}

frequency :: Float -> Exp Float
frequency = Expr.cons

asMono :: Id (vector Float)
asMono = id

type Id a = a -> a
asStereo :: Id (vector (Stereo.T Float))
asStereo = id

playStereoVector:: SVL.Vector (Stereo.T Float) -> IO ()
playStereoVector =
    void. SoxPlay.simple SVL.hPut SoxOption.none 44100


playStereo :: Sig.T (Stereo.T (NiceValue.T Float)) -> IO ()
playStereo sig =
    playStereoVector . ($ SVL.chunkSize 4000) =<<
    Render.run (Stereo.multiValue <$> sig)



playMono :: Sig.MV Float -> IO ()
playMono sig  =  playMonoVector . ($ SVL.chunkSize 10000) =<< Render.run sig

playMonoVector :: SVL.Vector Float -> IO ()
playMonoVector =
   void . SoxPlay.simple SVL.hPut SoxOption.none 44100

----------------------- Play Files -------------------------------------------------   
playFileMono :: FilePath -> IO ()
playFileMono fileName = do
    f <- Render.run id
    IO.withFile fileName IO.ReadMode $ \h -> playMonoVector . 
      f (SVL.chunkSize 100000).asMono .snd 
        =<< SVL.hGetContentsAsync (SVL.chunkSize 100000) h 


playFileStereo :: FilePath -> IO ()
playFileStereo fileName = do
    f <- Render.run id
    IO.withFile fileName IO.ReadMode $ \h -> playStereoVector . 
      f (SVL.chunkSize 100000). asStereo . snd 
        =<< SVL.hGetContentsAsync (SVL.chunkSize 100000) h

-------------- Helper Functions -----------------------------------------------------

-- Helper function for writing to an .f32 file
writeSignalToFileMono :: FilePath -> Sig.MV Float -> IO ()
writeSignalToFileMono fileName signal =
    (SVL.writeFile fileName . asMono =<<) $ fmap ($ SVL.chunkSize 10000000) $ Render.run  signal

-- Helper function for writing Chunks to an .f32 file. Keep take and chunk size equal.
writeChunkToFileMono :: FilePath -> Int -> Sig.MV Float -> IO ()
writeChunkToFileMono fileName len signal =
    (SVL.writeFile fileName . asMono . SVL.take len =<<) $ fmap ($ SVL.chunkSize 1000000) $ Render.run  signal

-- Helper function for vectorising into LazyVecs of parameteized size 
helperMonoSig2Vec :: Int -> Sig.MV Float -> IO (SVL.Vector Float)
helperMonoSig2Vec vsize sig = (\x -> ($ SVL.chunkSize vsize) =<< Render.run x ) $ sig

------------------------CREATE WAVE FILE------------------------------------------------------------------------

writeMonoSignalToWavFile :: FilePath -> Int-> Sig.MV Float  -> IO ()
writeMonoSignalToWavFile filepath psize sig   = do
            let wave =
                  Wav.Wave 
                     {  waveFileFormat = Wav.WaveVanilla,
                        waveSampleRate = 44100 :: Word32,            
                        waveSampleFormat = Wav.SampleFormatIeeeFloat32Bit,
                        waveChannelMask = Wav.speakerMono, --2 channels
                        waveDataOffset = 0,
                        waveDataSize = fromIntegral (128 :: Int) ,
                        waveSamplesTotal = fromIntegral (32 :: Int),
                        waveOtherChunks = []
                     }            

            Wav.writeWaveFile filepath wave $ \handle ->
               SVL.hPut handle . asMono . SVL.take psize =<< helperMonoSig2Vec 100000 sig 



