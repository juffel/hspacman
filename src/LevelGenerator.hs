module LevelGenerator where

import GameData
import Vector2D
import Prelude hiding(Left,Right)
import Data.Tuple

import Math.Matrix
--import Numeric.Probability.Distribution
import Control.Monad.Random
import System.Random

data Movement = Up | Down | Right | Left


{-data State s t = State { runState :: s -> (t,s) }
instance Monad (State s) where
	return val = State $ \st -> (val, st)
	-- State s a -> (a -> State s b) -> State s b
	-- (s -> (a,s)) -> ( a -> (s -> (b,s) ) ) -> ( s -> (b,s) )
	f1 >>= createf2 = State $ \s0 ->
		let (leftVal,leftState) =(runState f1) s0
		in let (State f2) = (createf2 leftVal)
			in f2 s0
-}

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

-- realizes a "torus like" behavior for positions on the field
getNeighbourIndex :: Size -> MatrIndex -> Movement -> MatrIndex
getNeighbourIndex (width,height) pos@(x,y) dir = case dir of
	Up -> (x,(y-1) `niceMod` width)
	--UpRight -> getNeighbourIndex field (getNeighbourIndex field pos Up) Right
	Right -> ((x+1) `niceMod` height, y)
	--DownRight -> getNeighbourIndex field (getNeighbourIndex field pos Down) Right
	Down -> (x,(y+1) `niceMod` width)
	--DownLeft -> getNeighbourIndex field (getNeighbourIndex field pos Down) Left
	Left -> ((x-1) `niceMod` height, y)
	--UpLeft -> getNeighbourIndex field (getNeighbourIndex field pos Up) Left
	where
		{-width = mGetWidth field
		height = mGetHeight field-}
		niceMod val m = case signum val of
			(-1) -> niceMod (val+m) m
			(1) -> val `mod` m
			(0) -> 0
			otherwise -> error "niceMod internal error!"


massiveField :: Size -> Labyrinth
massiveField (width,height) = mUnsafe (take height $ repeat lines)
	where
		lines = take width $ repeat Wall

genLabyrinth :: Size -> Int -> Labyrinth
genLabyrinth (width,height) seed = 
	circle $
	massiveField (width,height)
	where
		circle :: Labyrinth -> Labyrinth
		circle l = mapWithIndex makeCircle l
			where
				makeCircle pos  cell = 
					(if closeToTheEdge pos then Free else Wall)
					where 
						closeToTheEdge pos = 
							inBox ((1,1),(height-3,width-3)) pos && 
							(not $ inBox ((2,2),(height-5,width-5)) pos)

						{-
						closeToTheEdge pos = (\l -> length l > 0) $ filter (\diff -> ((vecX diff == 0) || (vecY diff ==0)) && ((abs (vecX diff) /=1 && abs (vecY diff) /=1 ) ) ) $ do
							edge <- [(1,1),(height-2,width-2)]
							let (xDiff,yDiff) = (pos <-> edge)
							return (xDiff,yDiff)
						xor a b = (a && not b) || (not a && b)
						-}


inBox :: Area -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox <+> sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
