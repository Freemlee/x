---------------------------------------------------------------------------
--	   The Hydra Computer Hardware Description Language
--	See the README file and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

module HDL.Hydra.Circuits.Comb where

import HDL.Hydra.Core.Signal
import HDL.Hydra.Core.Pattern


-- Unbuffered Fanout


fanout2 :: a -> (a,a)
fanout2 x = (x,x)

fanout3 :: a -> (a,a,a)
fanout3 x = (x,x,x)

fanout4 :: a -> (a,a,a,a)
fanout4 x = (x,x,x,x)

-- Duplicating a bit to form a word: fanout takes a wordsize k and a
-- signal x, and produces a word of size k each of whose bits takes
-- the value of x.

fanout :: Signal a => Int -> a -> [a]
fanout k x = take k (repeat x)


-- Buffered Fanout


fanoutbuf2 :: Signal a => a -> (a,a)
fanoutbuf2 x = (y,y)
  where y = buf x

fanoutbuf3 :: Signal a => a -> (a,a,a)
fanoutbuf3 x = (y,y,y)
  where y = buf x

fanoutbuf4 :: Signal a => a -> (a,a,a,a)
fanoutbuf4 x = (y,y,y,y)
  where y = buf x



-- Multiplexors


mux1 :: Signal a => a -> a -> a -> a
mux1 p a b = x
  where x = or2 (and2 (inv p) a) (and2 p b)

mux2 :: Signal a => (a,a) -> a -> a -> a -> a -> a
mux2 (p0,p1) a b c d = x
  where x = mux1 p0 (mux1 p1 a b) (mux1 p1 c d)

mux3 :: Signal a => (a,a,a) -> a -> a -> a -> a -> a-> a -> a -> a -> a
mux3 (c0,c1,c2) a0 a1 a2 a3 a4 a5 a6 a7 =
  mux1 c0
    (mux1 c1
      (mux1 c2 a0 a1)
      (mux1 c2 a2 a3))
    (mux1 c1
      (mux1 c2 a4 a5)
      (mux1 c2 a6 a7))

mux22 :: Signal a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
mux22 (p0,p1) (a0,a1) (b0,b1) (c0,c1) (d0,d1) = (x,y)
  where x = mux2 (p0,p1) a0 b0 c0 d0
        y = mux2 (p0,p1) a1 b1 c1 d1

mux1w :: Signal a => a -> [a] -> [a] -> [a]
mux1w c x y = map2 (mux1 c) x y

mux2w cc = map4 (mux2 cc)


-- Demultiplexors


demux1 :: Signal a => a -> a -> (a,a)
demux1 c x = (and2 (inv c) x, and2 c x)

demux2 :: Signal a => (a,a) -> a -> (a,a,a,a)
demux2 (c0,c1) x = (y0,y1,y2,y3)
  where (y0,y1) = demux1 c1 (inv c0)
        (y2,y3) = demux1 c1 c0

demux1w :: Signal a => [a] -> a -> [a]
demux1w [c0] x =
  let (a0,a1) = demux1 c0 x
  in [a0,a1]

demux2w :: Signal a => [a] -> a -> [a]
demux2w [c0,c1] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux1w [c1] a0
      w1 = demux1w [c1] a1
  in w0++w1

demux3w :: Signal a => [a] -> a -> [a]
demux3w [c0,c1,c2] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux2w [c1,c2] a0
      w1 = demux2w [c1,c2] a1
  in w0++w1

demux4w :: Signal a => [a] -> a -> [a]
demux4w [c0,c1,c2,c3] x =
  let (a0,a1) = demux1 c0 x
      w0 = demux3w [c1,c2,c3] a0
      w1 = demux3w [c1,c2,c3] a1
  in w0++w1



-- Logic on words


-- Word inverter: winv takes a word and inverts each of its bits

winv :: Signal a => [a] -> [a]
winv x = map inv x

-- And/Or over a word: Determine whether there exists a 1 in a word,
-- or whether all the bits are 0.  A tree fold can do this in log
-- time, but for simplicity this is just a linear time fold.

-- ?? any1, all1 deprecated, remove...

any0, any1, all0, all1 :: Signal a => [a] -> a
any1 = error "any1 replaced by orw" -- foldl or2 zero
all1 = error "all1 replaced by andw" -- foldl and2 one
all0 xs = inv (orw xs)
any0 xs = inv (andw xs)

orw, andw :: Signal a => [a] -> a
orw = foldl or2 zero
andw = foldl and2 one


-- Building a constant integer word

-- Representing a boolean bit as a word: boolword takes a bit x, and
-- pads it to the left with 0s to form a word.  If the input x is
-- False (0), the result is the integer 0 (i.e. n 0-bits), and if x is
-- True (1) the result is the integer 1 (rightmost bit is 1, all
-- others are 0).

boolword :: Signal a => Int -> a -> [a]
boolword n x = fanout (n-1) zero ++ [x]


-- Combinational shifting

-- Shift a word to the right (shr) or to the left (shl).  In both
-- cases, this is just a wiring pattern.  A 0 is brought in on one
-- side, and the bit on the other side is just thrown away.

shr x = zero : [x!!i | i <- [0..k-2]]
  where k = length x
shl x = [x!!i | i <- [1..k-1]] ++ [zero]
  where k = length x
