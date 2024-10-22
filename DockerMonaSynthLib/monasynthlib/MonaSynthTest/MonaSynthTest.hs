module Main where

import MonaSynthLib

import qualified LLVM.Core as LLVM
import qualified Synthesizer.LLVM.Causal.Process as Causal
import qualified Synthesizer.LLVM.Generator.Signal as Sig
import Synthesizer.LLVM.Causal.Process (($<), ($>), ($*), ($<#), ($*#))
import LLVM.DSL.Expression (Exp)
import Control.Concurrent.Async (async, wait, cancel, Async)



seq :: IO()
seq = playMono (Causal.take 22050 $* (monoGen Sine) (play c)<>
                 (Causal.take 22050 $* (monoGen Sine) (play d)) <>
                 (Causal.take 22050 $* (monoGen Sine) (play c)) <>
                 (Causal.take 22050 $* (monoGen Sine) (play dis)))

----------------------------------------------------------------------------------
-- plays 3 seconds at 44.1kHz of the chord C,E,A
playChord :: IO ()
playChord =
    playMono $ (Causal.take 132300 $*
        (Causal.amplify 1 $* applyEnvelop (createASREnv) (mixChord Saw [play "C4" , play "E4", play "A4"]) ))

-- write files:
test :: IO ()
test = do  
  -- Call other functions as needed, e.g. starting the mono orscillator
  -- writeMonoSignalToWavFile filepath sig psize
  --writeMonoSignalToWavFile "testSine100samples.wav"     (monoGen Sine 0.01)   310
  --writeMonoSignalToWavFile "testSaw100samples.wav"      (monoGen Saw 0.01)    310  
  --writeMonoSignalToWavFile "testTri100samples.wav"      (monoGen Tri 0.01)    310
  --writeMonoSignalToWavFile "testSqaure100samples.wav"   (monoGen Square 0.01) 310 
  --writeMonoSignalToWavFile "testTrapz100samples.wav"    (monoGen Trapz 0.01)  310 
  writeMonoSignalToWavFile "Release10820samples.wav" 20820 (Causal.take 208200 $* Sig.exponential2 5000 1)
  writeMonoSignalToWavFile "Attack10820samples.wav" 20820 (Causal.take 208200 $* 1-Sig.exponential2 5000 1) 
  writeMonoSignalToWavFile "AR-Envelope-40000samples.wav" 40000 (concatenateASRSignals 20000 20000)
 
main :: IO ()
main = do
    LLVM.initializeNativeTarget
    putStrLn "Enter 'Q' and press 'Enter' to exit"
    -- Start the filter sweep in a separate thread
    bkgTask <- async $ playChord
    -- Run the input listener in the main thread
    userInput bkgTask
    
    putStrLn "Test loading."