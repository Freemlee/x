module Add4 where

-- Define some adders at specific word sizes, manually specifying each
-- signal and component (a better approach, of course, is to use a
-- design pattern).

import HDL.Hydra.Core.Lib
import HDL.Hydra.Circuits.StandardCircuits

-- A 4-bit adder, constructed explicitly from 4 full adders.

rippleAdd4 :: Signal a => a -> [(a,a)] -> (a,[a])
rippleAdd4 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3)]
  = (c0, [s0,s1,s2,s3])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) cin

-- A 6-bit adder, extending rippleAdd4 in the obvious way.

rippleAdd6 :: Signal a => a -> [(a,a)] -> (a,[a])
rippleAdd6 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)]
  = (c0, [s0,s1,s2,s3,s4,s5])
  where
    (c0,s0) = fullAdd (x0,y0) c1
    (c1,s1) = fullAdd (x1,y1) c2
    (c2,s2) = fullAdd (x2,y2) c3
    (c3,s3) = fullAdd (x3,y3) c4
    (c4,s4) = fullAdd (x4,y4) c5
    (c5,s5) = fullAdd (x5,y5) cin

-- An 8-bit adder constructed from two 4-bit adders.  The signals are
-- named explicitly.

rippleAdd8 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3),
                (x4,y4),(x5,y5),(x6,y6),(x7,y7)]
  = (cout, [s0,s1,s2,s3,s4,s5,s6,s7])
  where (cout, [s0,s1,s2,s3])
           = rippleAdd4 cc [(x0,y0),(x1,y1),(x2,y2),(x3,y3)]
        (cc, [s4,s5,s6,s7])
           = rippleAdd4 cin [(x4,y4),(x5,y5),(x6,y6),(x7,y7)]
