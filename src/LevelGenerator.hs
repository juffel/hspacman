module LevelGenerator(
	genLabyrinth
) where

import GameData hiding(Direction)
import Vector2D
import Prelude hiding(Left,Right)
import Data.Tuple

import Math.Matrix
--import Numeric.Probability.Distribution
import Control.Monad.Random
import System.Random


type View a st = (Matrix a, Pos, st)
type Behaviour a st = (View a st -> (a,Maybe Movement,st))

calcNewMatr :: Behaviour a st -> Pos -> Matrix a -> st -> Matrix a
calcNewMatr beh pos0 matr st =
	let
		(newVal,dir,newSt) = beh (matr, pos0,st)
		newMatr = mSet (swap pos0) newVal matr 
	in case dir of 
		Nothing -> newMatr
		Just dir' -> calcNewMatr beh newPos newMatr newSt
			where 
				newPos = getNeighbourIndex (mGetWidth matr, mGetHeight matr) pos0 dir'

--rndProp 

--type TunnelState = 

makeTunnel :: (RandomGen g) => Rand g Movement -> Pos -> Matrix Territory -> g -> Matrix Territory
makeTunnel distr pos0 matr gen = calcNewMatr beh pos0 matr gen
	where
		--beh :: (RandomGen g') => Behaviour Territory g'
		--beh :: Behaviour Territory g
		beh (matr,pos,gen') = (Free,movement,newGen)
			where
				movement =
					if (mGet pos matr) == Wall
					then Just dir
					else Nothing
				{-
					if vecX pos < mGetWidth matr
					then Just Right
					else Nothing
				-}
				(dir,newGen) = runRand distr gen'

randomList :: (RandomGen gen) => Rand gen Movement
randomList = fromList [(Up,0.05),(Down,0.05),(Left,0.05),(Right,0.85)]

{-
-- calculates a list of random values from a given distribution
weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
	where m = sequence . repeat . fromList $ weights
-}

massiveField :: Size -> Labyrinth
massiveField (width,height) = mUnsafe (take height $ repeat lines)
	where
		lines = take width $ repeat Wall

genLabyrinth :: Size -> Int -> Labyrinth
genLabyrinth (width,height) seed = 
	
	massiveField (width,height)

inBox :: Area -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox <+> sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
