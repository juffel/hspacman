module Vector2D where

type Vec a = (a,a)

-- some useful functions for working with vectors:
vecX :: Vec a -> a
vecX = fst
vecY :: Vec a -> a
vecY = snd

-- vector operations:
infix 8 <+> -- vector addition
infix 8 <-> -- vector subtraction
infix 9 <*> -- component-by-component multiplication (!)
infix 9 </> -- component-by-component division (!)
infix 9 *> -- scalar mult
infix 9 <* -- scalar mult
infix 9 /> -- scalar div
infix 9 </ -- scalar div
--(:+:) :: (Num a) => Vec a -> Vec a -> Vec a
l <+> r = (vecX l + vecX r,  vecY l + vecY r)
l <-> r = (vecX l - vecX r,  vecY l - vecY r)
l <*> r = (vecX l * vecX r,  vecY l * vecY r)
l </> r = (vecX l / vecX r,  vecY l / vecY r)
scalar *> vec = (scalar * (vecX vec), scalar * (vecY vec))
(<*) = flip (*>)
scalar /> vec = (scalar / (vecX vec), scalar / (vecY vec))
vec </ scalar = ((vecX vec) / scalar, (vecY vec) / scalar)
