module Traffic2 where

{-Circuit for the second exercise in Assessed Exercise 1 -}

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

go :: Clocked a => a -> a
go x = y
  where
	y = dff (or2 x y)

mainCircuit :: Clocked a => a -> (a,a,a)
mainCircuit x = (r,a,g)
  where
	y = dff (or2 x y)
	countedVal = resetCounter y storedVal
	storedVal = wlatch 4 countedVal
	(r,a,g) = lightLogic storedVal

bitToWord :: Clocked a => a -> [a]
bitToWord x = [a,b,c,d]
  where
	a = zero
	b = zero
	c = zero
	d = x
	
add4 :: Clocked a => [a] -> a -> [a]
add4 [i0,i1,i2,i3] count = [o0,o1,o2,o3]
  where 
	(c3,o3) = halfAdd count i3
	(c2,o2) = halfAdd c3 i2
	(c1,o1) = halfAdd c2 i1
	(c0,o0) = halfAdd c1 i0
	
resetCounter :: Clocked a => a -> [a] -> [a]
resetCounter x [a,b,c,d] = out
  where
	r = and2 a d
	out = mux1w r (add4 [a,b,c,d] x) [zero,zero,zero,x]
	
lightLogic :: Clocked a => [a] -> (a,a,a)
lightLogic [o3,o2,o1,o0] = (r,a,g)
  where
	g = or3 (and3 (inv o3) o2 o1) (and3 (inv o3) o2 o0) (and4 o3 (inv o2) (inv o1) (inv o0))
	a = or2 (and4 (inv o3) o2 (inv o1) (inv o0)) (and4 o3 (inv o2) (inv o1) o0)
	r = or2 (and3 (inv o3) (inv o2) o1) (and3 (inv o3) (inv o2) o0)