module LevelGenerator(
	genLabyrinth
) where

import GameData --hiding(Direction)
import Vector2D
import Prelude hiding(Left,Right)
import Data.Tuple
import qualified Data.Foldable as F

import Math.Matrix
--import Numeric.Probability.Distribution
import Control.Monad.Random
import System.Random


-- create a labyrinth by spawning worms on a field that is massive in the beginning:
genLabyrinth :: Size -> Float -> Int -> Labyrinth
genLabyrinth (width,height) wallRatio seed = 
	evalRand (randomTunnels startMatrix wallRatio) (mkStdGen seed)
		where
			startMatrix = massiveField (width,height)

-- a field with wall on all cells 
massiveField :: Size -> Labyrinth
massiveField (width,height) = mUnsafe (take height $ repeat lines)
	where
		lines = take width $ repeat Wall

-- bore tunnels until the wall ratio has been reached:
randomTunnels :: (RandomGen g) => Labyrinth -> Float -> Rand g Labyrinth
randomTunnels lab wallRatio = if currentWallRatio <= wallRatio then return lab else do
	randomPos <- fromList $ zip (map swap $ mGetAllIndex lab) (repeat 1)
	let oneStepLeft = (getNeighbourIndex (mGetWidth lab,mGetHeight lab) randomPos Left)
	lab' <- boreTunnel randomPos Right lab
	lab'' <- boreTunnel oneStepLeft Left lab'
	randomTunnels lab'' wallRatio -- <- recursion !
	where
		currentWallRatio = (fromIntegral countWall) / (fromIntegral $ width*height)
		countWall = F.foldl (+) 0 (fmap fromEnum lab)
		(width,height) = (mGetWidth lab, mGetHeight lab)
		--toInt Wall = 1
		--toInt Free = 0

-- bore one tunnel:
boreTunnel pos0 favDir matr = calcNewMatr (wormBehaviour (favDir,0.95)) pos0 matr WS{ lastDir=favDir }

-- |this is what a worm sees at every iteration:
--type View a st = (Matrix a, Pos)
type View st a = (Matrix a, Pos,st)

-- |this defines a worms behaviour:
--type Behaviour a st = (View a st -> (a,Maybe Movement,st))
type Behaviour st g a = (View st a -> Rand g (a,Maybe Movement,st))


{- allows a simple automaton ("worm") to work on a matrix
a "worm" is defined by its Behaviour...
-}
calcNewMatr beh pos0 matr st = do
	(newMatr, mov, st') <- oneStep beh pos0 matr st
	case mov of
		Nothing -> return newMatr
		Just dir -> calcNewMatr beh newPos newMatr st'
			where
				newPos = getNeighbourIndex (mGetWidth matr, mGetHeight matr) pos0 dir

-- |let a "worm" take exactly one step:
oneStep :: (Behaviour st g a) -> Pos -> Matrix a -> st -> Rand g ((Matrix a), Maybe Movement,st)
oneStep beh pos0 matr st = do
	(newVal,dir,st') <- beh (matr, pos0,st)
	let newMatr = mSet (swap pos0) newVal matr 
	return (newMatr,dir,st')


data WormStatus = WS {
	lastDir :: Direction
}

{- defines the behaviour of a worm. The worm will roughly go to its favourite direction,
-- until it reaches a free field.
-}
wormBehaviour :: (RandomGen g) => (Direction,Rational) -> (Behaviour WormStatus g Territory)
wormBehaviour dirAndProp@(favDir,_) (mat,pos,WS{ lastDir=lastDir }) = do
	rndDir <- randomDirS dirAndProp
	return $ (Free,maybeDir rndDir,WS{ lastDir = rndDir })
	where
		maybeDir rndDir = if 
			(mGet (swap forwardPos) mat)/=Free &&
			(mGet (swap leftPos) mat)/=Free &&
			(mGet (swap rightPos) mat)/=Free 
			then Just $ rndDir
			else Nothing
		[forwardPos,leftPos,rightPos] = map (getNeighbourIndex (mGetWidth mat,mGetHeight mat) pos) [lastDir, leftOf lastDir, rightOf lastDir]


-- returns a distribution of directions, given any "favourite" direction:
randomDirS :: (RandomGen gen) => (Direction,Rational) -> Rand gen Movement
randomDirS (preference,prob) = fromList $ (preference,prob) :
	zip (orthogonal preference) (repeat $ (1-prob)/2)


{-
-- calculates a list of random values from a given distribution
weightedList :: RandomGen g => g -> [(a, Rational)] -> [a]
weightedList gen weights = evalRand m gen
	where m = sequence . repeat . fromList $ weights
-}


-- tests if a position is inside a given area:
inBox :: Area -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox <+> sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
