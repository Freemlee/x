module Main where

-- Run various binary adder circuits, including several that are
-- defined at specific word sizes (4, 6, 8 bits) and the general k-bit
-- ripple carry adder instanciated at 16 bits.

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import Add4

------------------------------------------------------------------------
-- Test data

test_data_add4 =
--  c  x  y
  [[0,  5,  8],
   [1,  2, 12]]

test_data_add6 =
--  c  x  y
  [[0,  5,  8],
   [1,  2, 12],
   [0, 41, 13]]

test_data_add8 =
--  c    x    y
  [[0,   5,   8],
   [1,   2,  12],
   [0,  41,  13],
   [0, 103,  59],
   [0, 178, 193],
   [1,  17, 209]]

test_data_add16 =
--  c      x      y
  [[0,     5,     8],
   [1,     2,    12],
   [0,    41,    13],
   [0,   103,    59],
   [0,   178,   193],
   [0,  9037, 20185],
   [0, 31000, 32000],
   [0, 51000, 40000],
   [1,    17,   209]]

------------------------------------------------------------------------
-- main program

separator :: IO ()
separator = putStrLn (take 72 (repeat '-'))

main :: IO ()
main =
  do separator
     putStrLn "4-bit adder"
     run_adder rippleAdd4 4 test_data_add4

     separator
     putStrLn "6-bit adder"
     run_adder rippleAdd6 6 test_data_add6

     separator
     putStrLn "8-bit adder"
     run_adder rippleAdd8 8 test_data_add8

     separator
     putStrLn "16-bit adder"
     run_adder rippleAdd 16 test_data_add16

     separator

------------------------------------------------------------------------
-- Test bench

type Bit = Stream Bool
type Word = [Bit]

run_adder
  :: (Bit -> [(Bit,Bit)] -> (Bit,Word))
  -> Int
  -> [[Int]]
  -> IO ()

run_adder adder k input = runAllInput input output
  where
    cin = getbit   input 0
    x   = getbin k input 1
    y   = getbin k input 2

    (cout,s) = adder cin (zip x y)

    output =
      [string "  x = ", bindec 6 x,
       string "  y = ", bindec 6 y,
       string "  cin = ", bit cin,
       string "     ==>     cout = ", bit cout,
       string "  s = ", bindec 6 s]
