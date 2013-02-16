module Main where

-- Run various binary adder circuits, including several that are
-- defined at specific word sizes (4, 6, 8 bits) and the general k-bit
-- ripple carry adder instanciated at 16 bits.

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Traffic2

------------------------------------------------------------------------
-- Test data

test_data_basicIn = 
  [[0],
  [0],
  [0],
  [0],
  [1],
  [0],
  [0],
  [1],
  [0],
  [0],
  [1],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [0]]
  
--counter data
counter_test = 
  [[0],[1],[2],[3],[4],[5],[6],[7],[8],[9]]

------------------------------------------------------------------------
-- main program

separator :: IO ()
separator = putStrLn (take 72 (repeat '-'))

main :: IO ()
main =
  do separator
     putStrLn "Reset Pressed"
     run_go mainCircuit test_data_basicIn

     separator
     putStrLn "Test Counter Reset"
     run_resetCounter resetCounter counter_test
     
     separator
     putStrLn "Light Logic"
     run_testLights lightLogic counter_test

------------------------------------------------------------------------
-- Test bench

type Bit = Stream Bool
type Word = [Bit]

run_go
  :: (Bit -> (Bit,Bit,Bit))
  -> [[Int]]
  -> IO ()

run_go func input = runAllInput input output
  where
    sig = getbit   input 0
    (x,y,z) = func sig

    output =
      [bit sig, bit x, bit y, bit z]

run_resetCounter :: (Bit -> Word -> Word) -> [[Int]] -> IO()
run_resetCounter func input = runAllInput input output
  where
    inp = getbin 4 input 0
    out = func one inp

    output =
      [bindec 4 inp, string "   ", bindec 4 out]
      
run_testLights :: (Word -> (Bit,Bit,Bit)) -> [[Int]] -> IO()
run_testLights func input = runAllInput input output
  where
	inp = getbin 4 input 0
	(r,a,g) = func inp
	
	output =
	  [string "counter =", bindec 4 inp, string " RAG values = ",bit r,bit a,bit g]