module Main where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits
import TrafficLights

test_signal =
  [[0],
  [1],
  [2],
  [3],
  [4],
  [5],
  [6],
  [7],
  [8],
  [9]]
  
test_counter =
  [[0,0,0],
  [0,1,1],
  [0,0,4],
  [0,1,4],
  [1,0,4],
  [1,1,4]]
  
test_controller =
  [[0,0],
  [0,0],
  [1,0],
  [0,1],
  [0,2],
  [0,3],
  [0,4],
  [0,5],
  [1,6],	--the 1 should have no effect
  [0,7],
  [0,8],
  [0,9]
  ]
  
test_controller2 =
  [[0],
  [0],
  [1],
  [0],
  [0],
  [0],
  [0],
  [0],
  [1],	--the 1 should have no effect
  [0],
  [0],
  [0],
  [0],
  [0],
  [0],
  [1],	--the 1 should have no effect
  [0],
  [0],
  [0]
  ]

separator :: IO ()
separator = putStrLn (take 72 (repeat '-'))

main :: IO ()
main =
  do separator
     putStrLn "Test Helpers"
     separator
     putStrLn "Not Zero"
     run_not_zero notZero test_signal
     separator
     separator
     putStrLn "Test Important Components"
     separator
     separator
     putStrLn "Traffic Light Values"
     run_lights lightLogic test_signal
     separator
     putStrLn "Counter Values"
     run_counter counter2 test_counter
     separator
     putStrLn "Traffic Lights 1"
     run_controller controller1 test_controller
     putStrLn "Traffic Lights 2"
     run_controller2 controller2 test_controller2
     --run_controller controller1 test_controller
     separator
     
  


type Bit = Stream Bool
type Word = [Bit]

run_should_increment ::[[Int]] -> IO()
run_should_increment input = runAllInput input output
  where
	res = getbit input 0
	i = getbin 4 input 1
	shouldReset = reset res i
	o = and2 (increment i) (inv shouldReset)
	output = 
	  [bit res, bindec 4 i, bit o]

run_lights :: (Word -> (Bit,Bit,Bit)) -> [[Int]] -> IO()
run_lights func input = runAllInput input output
  where
	i = getbin 4 input 0
	
	(r,a,g) = func i
	
	output = 
	  [bindec 4 i, string "  r=", bit r,
	  string "  a =", bit a,
	  string "  g =", bit g]
	  
run_counter :: (Bit -> Bit -> Word -> Word) -> [[Int]] -> IO()
run_counter func input = runAllInput input output
  where
	reset = getbit input 0
	increment = getbit input 1
	i = getbin 4 input 2
	o = func reset increment i
	output =
	  [string "reset=", bit reset, string " increment=", bit increment, 
	  string " value before=", bindec 4 i, string " value after =", bindec 4 o]
	  
run_not_zero :: (Word -> Bit) -> [[Int]] -> IO()
run_not_zero func input = runAllInput input output
  where
	i = getbin 4 input 0
	o = func i
	output =
	  [string "input=", bindec 4 i, string " output=", bit o]
	  
run_controller :: (Bit -> Word -> (Bit,Bit,Bit)) -> [[Int]] -> IO()
run_controller func input = runAllInput input output
  where
	but = getbit input 0
	i = getbin 4 input 1
	(r,a,g) = func but i
	output =
	  [bit but, string " value=", bindec 4 i, string " r=",
	  bit r, string " a=",bit a,string " g=",bit g] 
	  
run_controller2 :: (Bit -> (Bit,Bit,Bit)) -> [[Int]] -> IO()
run_controller2 func input = runAllInput input output
  where
	buttonPress = getbit input 0
	(r,a,g) = func buttonPress
	output =
	  [string "button=", bit buttonPress, string "RAG = ", bit r, bit a, bit g] 