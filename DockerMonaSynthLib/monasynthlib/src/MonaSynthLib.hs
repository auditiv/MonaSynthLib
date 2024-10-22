{-# LANGUAGE ScopedTypeVariables #-}
module MonaSynthLib
    ( -- * Generate Waveshapes
    module Generators,
    -- * Play sounds
    module Play,

    -- * Notes
    module Notes,

    -- * Filters
    module Filters,

    -- * Plot sounds
    module Mixers,

    -- * Save and load sounds
    module Multithreading
  )
where

import MonaSynthLib.Generators as Generators
import MonaSynthLib.Notes as Notes
import MonaSynthLib.Play as Play
import MonaSynthLib.Filters as Filters
import MonaSynthLib.Mixers as Mixers
import MonaSynthLib.Multithreading as Multithreading

import qualified Data.StorableVector.Lazy as SVL

import Control.Concurrent.Async (async, wait, cancel, Async)
import Control.Concurrent (threadDelay, forkIO, Chan, newChan, writeChan, readChan, dupChan, MVar,newMVar, readMVar, modifyMVar_,newEmptyMVar, putMVar, takeMVar, forkIO, killThread, ThreadId)
--import Control.Concurrent (forkIO, threadDelay,readMVar,newMVar, MVar, newEmptyMVar,modifyMVar_, putMVar, tryTakeMVar)
import Control.Monad (forever, unless)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)
import System.Exit (exitSuccess) -- Import exitSuccess to quit the program
-- minimal LLVM and llvm-synthesizer bindings
import qualified LLVM.Core as LLVM
import qualified Synthesizer.LLVM.Causal.Process as Causal
import LLVM.DSL.Expression (Exp, (>*), (&&*))
import Synthesizer.LLVM.Causal.Functional (($&), (&|&))
import Synthesizer.LLVM.Causal.Process (($<), ($>), ($*), ($<#), ($*#))

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
-- simplifies all things:
import Type.Data.Num.Decimal (D4, D8, d8)
import qualified Algebra.Ring as Ring
import qualified Synthesizer.Generic.Control as Wave
import Data.Ratio
--for writing .wav-Files
import Codec.Audio.Wave as Wav
import GHC.Word (Word32)




