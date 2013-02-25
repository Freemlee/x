---------------------------------------------------------------------------
--	   The Hydra Computer Hardware Description Language
--	See the README file and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

module HDL.Hydra.Circuits.Memory where
import HDL.Hydra.Core.Signal
import HDL.Hydra.Circuits.Comb
import HDL.Hydra.Circuits.Register

mem1 :: (Signal a, Clocked a) => Int
  -> a -> [a] -> [a] -> a -> a

mem1 0 ld d sa x = reg1 ld x

mem1 (k+1) ld (d:ds) (sa:sas) x = a
  where
    (ld0,ld1) = demux1 d ld
    a0 = mem1 k ld0 ds sas x
    a1 = mem1 k ld1 ds sas x
    a = mux1 sa a0 a1


{-
mem1 :: Clocked a => 
mem1 0 sto ps x = membit fet sto x
mem1 (k+1) sto (p:ps) =
  let m0 = mem1 k (and2 (inv p) sto) ps
      m1 = mem1 k (and2 p sto) ps
  in mux1
-}

membit sto x = s
  where s = dff (mux1 sto s x)

memw :: Clocked a => Int -> Int -> a -> [a] -> [a] -> [a]
memw n k sto p x =
  [mem1a k sto p (x!!i) | i <- [0..n-1]]

mem1a :: Clocked a => Int -> a -> [a] -> a -> a
mem1a 0 sto p x = reg1 sto x
mem1a (k+1) sto (p:ps) x =
  let (sto0,sto1) = demux1 p sto
      m0 = mem1a k sto0 ps x
      m1 = mem1a k sto1 ps x
  in mux1 p m0 m1
