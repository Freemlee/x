module TrafficLights where

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

-- input button
controller1 :: Clocked a => a -> [a] -> (a,a,a)
controller1 button signal = lightLogic counterVal
  where
	{-
	shouldReset = reset button signal
	nextVal = counter4 storedVal
	--shouldIncrement = and2 (increment signal) (inv shouldReset)
	--counterVal = counter2 shouldReset shouldIncrement signal
	updatedCount = counter2 reset should
	stored = wlatch 4 
	reset = isNine stored
  -}
isNine :: Clocked a => [a] -> a
isNine [a,b,c,d] = output
  where
	output = and2 b d

ff :: Clocked a => a -> (a,a,a)
ff x = (r,a,g)
	where
	(r,a,g) = lightLogic previousSignal 
	wlatch (resetPressed x)
		

	
controller2 :: Clocked a => a -> (a,a,a)
controller2 button = lightLogic counterVal
  where
	shouldIncrement = increment2 button
	counterVal = counter2 shouldReset shouldIncrement [zero,zero,zero,zero]
	shouldReset = reset2 counterVal
	
counter4 :: Clocked a => a -> [a] -> [a]
counter4 cin [x0, x1, x2, x3] 
        = [s0, s1, s2, s3]
        where
                (cOut,s0) = halfAdd x0 c1
                (c1,s1) = halfAdd x1 c2
                (c2,s2) = halfAdd x2 c3
                (c3,s3) = halfAdd x3 cin

signalExtender :: Clocked a => a -> [a]
signalExtender x = [zero,zero,zero,x]

notZero :: Clocked a => [a] -> a
notZero [a,b,c,d] = output
  where
	output = inv (or4 a b c d)
	
reset :: Clocked a => a -> [a] -> a
reset button [i3,i2,i1,i0] = out
  where
	x = and2 (notZero [i3,i2,i1,i0]) button
	y = and2 i3 i0 -- if value is 9
	out = or2 x y
	
reset2 :: Clocked a => [a] -> a
reset2 [i3,i2,i1,i0] = and2 i3 i0
	
increment2 :: Clocked a => a -> a
increment2 x = y
  where
	y = dff (or2 x y)
	
increment :: Clocked a => [a] -> a
increment sig = out
  where
	out = (inv (notZero sig))

lightLogic :: Clocked a => [a] -> (a,a,a)
lightLogic [o3,o2,o1,o0] = (r,a,g)
  where
	g = or3 (and3 (inv o3) o2 o1) (and3 (inv o3) o2 o0) (and4 o3 (inv o2) (inv o1) (inv o0))
	a = or2 (and4 (inv o3) o2 (inv o1) (inv o0)) (and4 o3 (inv o2) (inv o1) o0)
	r = or2 (and3 (inv o3) (inv o2) o1) (and3 (inv o3) (inv o2) o0)
	
{-
counter :: Clocked a => [a] -> a -> [a]
counter [i0,i1,i2,i3] cin = [o0,o1,o2,o3]
  where 
	(c3,o3) = halfAdd cin i3
	(c2,o2) = halfAdd c3 i2
	(c1,o1) = halfAdd c2 i1
	(c0,o0) = halfAdd c1 i0
	-}

{- A 4 bit register with inputs reset, increment and 4 bit word. If reset = 1 output is equal to
the 4 bit word 1, if increment is equal to one the word adds 1 to it. If both are one then reset
trumps -}
counter2 :: Clocked a => a -> a -> [a] -> [a]
counter2 reset increment [i3,i2,i1,i0] = out
  where
	(c3,t3) = halfAdd i3 c2
	(c2,t2) = halfAdd i2 c1
	(c1,t1) = halfAdd i1 c0
	(c0,t0) = halfAdd i0 increment
	out = [mux1 reset i3 zero, mux1 reset i2 zero, mux1 reset i1 zero, mux1 reset i0 one]
