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


--type View a st = (Matrix a, Pos)
type View st a = (Matrix a, Pos,st)
--type Behaviour a st = (View a st -> (a,Maybe Movement,st))
type Behaviour st g a = (View st a -> Rand g (a,Maybe Movement,st))


oneStep :: (Behaviour st g a) -> Pos -> Matrix a -> st -> Rand g ((Matrix a), Maybe Movement,st)
oneStep beh pos0 matr st = do
	(newVal,dir,st') <- beh (matr, pos0,st)
	let newMatr = mSet (swap pos0) newVal matr 
	return (newMatr,dir,st')

calcNewMatr randomBeh pos0 matr st = do
	(newMatr, mov, st') <- oneStep randomBeh pos0 matr st
	case mov of
		Nothing -> return newMatr
		Just dir -> calcNewMatr randomBeh newPos newMatr st'
			where
				newPos = getNeighbourIndex (mGetWidth matr, mGetHeight matr) pos0 dir

data WormStatus = WS {
	lastDir :: Direction
}

wormBehaviour :: (RandomGen g) => (Direction,Rational) -> (Behaviour WormStatus g Territory)
wormBehaviour dirAndProp@(favDir,_) (mat,pos,WS{ lastDir=lastDir }) = do
	--randomDir <- randomDirS favDir 
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

	--nextPos = getNeighbourIndex (width,height) randomDir
	--return $ \(mat,pos) -> (Free,calcNewDir favDir randomDir mat pos)

calcNewDir favDir randomDir mat pos =
	if (mGet (swap pos) mat)/=Free
	then Just randomDir
	else Nothing
		where
			--[posNext] 
			--[posL,posR] = map (getNeighbourIndex (mGetWidth matr,mGetHeight matr) pos) (orthogonal favDir)


-- returns a distribution of directions, given any "favourite" direction:
randomDirS :: (RandomGen gen) => (Direction,Rational) -> Rand gen Movement
randomDirS (preference,prob) = fromList $ (preference,prob) :
	zip (orthogonal preference) (repeat $ (1-prob)/2)
--map (\x -> (x,(1 - prob)/2)) (orthogonal preference)

opposite :: Direction -> Direction
opposite Left = Right
opposite Right = Left
opposite Up = Down
opposite Down = Up

leftOf Left = Down
leftOf Up = Left
leftOf Right = Up
leftOf Down = Right

rightOf Left = Up
rightOf Up = Right
rightOf Right = Down
rightOf Down = Left

orthogonal :: Direction -> [Direction]
orthogonal d = [ ret | ret<-allDirs, ret/=d, ret/=opposite d ]

allDirs = [Up,Down,Left,Right]

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

genLabyrinth :: Size -> Float -> Int -> Labyrinth
genLabyrinth (width,height) wallRatio seed = 
	evalRand (randomTunnels startMatrix wallRatio) (mkStdGen seed)
		where
			startMatrix = massiveField (width,height)

randomTunnels :: (RandomGen g) => Labyrinth -> Float -> Rand g Labyrinth
randomTunnels lab wallRatio = if currentWallRatio <= wallRatio then return lab else do
	randomPos <- fromList $ zip (map swap $ mGetAllIndex lab) (repeat 1)
	--let lab' = lab
	lab' <- boreTunnel randomPos Right lab
	lab'' <- boreTunnel (getNeighbourIndex (mGetWidth lab,mGetHeight lab) randomPos Left) Left lab'
	--let lab'' = lab'
	--return lab''
	randomTunnels lab'' wallRatio
	where
		currentWallRatio = (fromIntegral countWall) / (fromIntegral $ width*height)
		countWall = F.foldl (+) 0 (fmap toInt lab)
		(width,height) = (mGetWidth lab, mGetHeight lab)
		toInt Wall = 1
		toInt Free = 0

boreTunnel pos0 favDir matr = calcNewMatr (wormBehaviour (favDir,0.95)) pos0 matr WS{ lastDir=favDir }

inBox :: Area -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox <+> sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
