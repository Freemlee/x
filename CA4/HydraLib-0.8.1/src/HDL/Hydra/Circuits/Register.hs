---------------------------------------------------------------------------
--	   The Hydra Computer Hardware Description Language
--	See the README file and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

-- | A collection of register circuits.

module HDL.Hydra.Circuits.Register
  (

-- * Basic registers

-- | A register is a circuit with an internal state,
-- and with the ability to load an external value into the
-- state and to read out the state.

-- ** Bit registers

    reg1





-- ** Word registers

  , reg
  , wlatch

-- * Shift registers

-- ** Shifting left to right

-- ** Shifting right to left


-- * Register files

-- ** Bit register file

  , regfile1

-- ** Word register file

  , regfile
  )
  where

import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.Pattern
import HDL.Hydra.Circuits.Comb




-- | /1-bit register./  The reg1 circuit contains a state of one
-- bit.  Example:
--
-- @
-- y = /reg1/ ld x
-- ld = ...
-- x = and2 a b
-- @
--
-- The reg1 is used as a building block for word registers.

reg1 :: Clocked a => a -> a -> a
reg1 ld x = r
  where r = dff (mux1 ld r x)



reg
  :: Clocked a =>
  Int             -- ^ k = the word size
  -> a          -- ^ ld = the load control signal
  -> [a]        -- ^ input word of size k
  -> [a]        -- ^ output is the register state


reg k ld x = mapn (reg1 ld) k x

wlatch :: Clocked a => Int -> [a] -> [a]
wlatch k x = mapn dff k x







-- | The 1-bit register file.
regfile1 :: Clocked a => Int -> a -> [a] -> [a] -> [a] -> a -> (a,a)

regfile1 0 ld d sa sb x = (r,r)
  where r = reg1 ld x

regfile1 (k+1) ld (d:ds) (sa:sas) (sb:sbs) x = (a,b)
  where
    (a0,b0) = regfile1 k ld0 ds sas sbs x
    (a1,b1) = regfile1 k ld1 ds sas sbs x
    (ld0,ld1) = demux1 d ld
    a = mux1 sa a0 a1
    b = mux1 sb b0 b1

regfile :: Clocked a => Int -> Int
  -> a -> [a] -> [a] -> [a] -> [a] -> ([a],[a])

regfile n k ld d sa sb x =
   unbitslice2 [regfile1 k ld d sa sb (x!!i)  | i <- [0..n-1]]

