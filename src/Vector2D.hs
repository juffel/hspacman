module Vector2D where

import Data.Tuple

type Vec a = (a,a)

-- some useful functions for working with vectors:
vecX :: Vec a -> a
vecX = fst
vecY :: Vec a -> a
vecY = snd
fOnVec f vec = (f $ vecX vec, f $ vecY vec)

-- vector operations:
infixl 8 <+> -- vector addition
infixl 8 <-> -- vector subtraction
infixl 9 <*> -- component-by-component multiplication (!)
infixl 9 </> -- component-by-component division (!)
infixl 9 *> -- scalar mult
infixl 9 <* -- scalar mult
infixl 9 /> -- scalar div
infixl 9 </ -- scalar div
--(:+:) :: (Num a) => Vec a -> Vec a -> Vec a
l <+> r = (vecX l + vecX r,  vecY l + vecY r)
l <-> r = (vecX l - vecX r,  vecY l - vecY r)
l <*> r = (vecX l * vecX r,  vecY l * vecY r)
l </> r = (vecX l / vecX r,  vecY l / vecY r)
scalar *> vec = (scalar * (vecX vec), scalar * (vecY vec))
(<*) = flip (*>)
scalar /> vec = (scalar / (vecX vec), scalar / (vecY vec))
vec </ scalar = ((vecX vec) / scalar, (vecY vec) / scalar)

transpose :: Vec a -> Vec a
transpose x = swap x 
