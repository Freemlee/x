---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
---------------------------------------------------------------------------

> module HDL.Hydra.Circuits.Add where

> import HDL.Hydra.Core.Signal
> import HDL.Hydra.Core.Pattern


---------------------------------------------------------------------------
				Adder
---------------------------------------------------------------------------

Bit addition

> halfAdd :: Signal a => a -> a -> (a,a)
> halfAdd x y = (and2 x y, xor2 x y)

> bsum, bcarry :: Signal a => (a,a) -> a -> a
> bsum (x,y) c = xor3 x y c
> bcarry (x,y) c = or3 (and2 x y) (and2 x c) (and2 y c)

> fullAdd :: Signal a => (a,a) -> a -> (a,a)
> fullAdd (x,y) c = (bcarry (x,y) c, bsum (x,y) c)


Ripple carry addition

> rippleAdd :: Signal a => a -> [(a,a)] -> (a,[a])
> rippleAdd = mscanr fullAdd


Two's complement addition and subtraction

> addSub :: Signal a => a -> [(a,a)] -> (a,[a])
> addSub sub xy = rippleAdd sub (map f xy)
>   where f (x,y) = (x, xor2 sub y)

---------------------------------------------------------------------------
			      Comparison
---------------------------------------------------------------------------

Bit comparison

> cmp1 :: Signal a => (a,a,a) -> (a,a) -> (a,a,a)
> cmp1 (lt,eq,gt) (x,y) =
>   (or2 lt (and3 eq (inv x) y),
>    and2 eq (inv (xor2 x y)),
>    or2 gt (and3 eq x (inv y))
>   )

Ripple comparison

> rippleCmp :: Signal a => [(a,a)] -> (a,a,a)
> rippleCmp = foldl cmp1 (zero,one,zero)
