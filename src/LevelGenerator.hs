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
type View a = (Matrix a, Pos)
--type Behaviour a st = (View a st -> (a,Maybe Movement,st))
type Behaviour a = (View a -> (a,Maybe Movement))


oneStep :: Rand g (Behaviour a) -> Pos -> Matrix a -> Rand g ((Matrix a), Maybe Movement)
oneStep randomBeh pos0 matr = do
	beh <- randomBeh
	let
		(newVal,dir) = beh (matr, pos0)
		newMatr = mSet (swap pos0) newVal matr 
	--return (matr,Just Right)
	return (newMatr,dir)

calcNewMatr randomBeh pos0 matr = do
	(newMatr, mov) <- oneStep randomBeh pos0 matr
	case mov of
		Nothing -> return newMatr
		Just dir -> calcNewMatr randomBeh newPos newMatr
			where
				newPos = getNeighbourIndex (mGetWidth matr, mGetHeight matr) pos0 dir


wormBehaviour :: (RandomGen g) => (Direction,Rational) -> Rand g (Behaviour Territory)
wormBehaviour favDir = do
	randomDir <- randomDirS favDir 
	return $ \(mat,pos) -> (Free,calcNewDir randomDir mat pos)

calcNewDir randomDir mat pos = case (mGet (swap pos) mat) of
	Free -> Nothing
	_ -> Just randomDir

opposite :: Direction -> Direction
opposite Left = Right
opposite Right = Left
opposite Up = Down
opposite Down = Up

-- returns a distribution of directions, given any "favourite" direction:
randomDirS :: (RandomGen gen) => (Direction,Rational) -> Rand gen Movement
randomDirS (preference,prob) = fromList $ (preference,prob) :
	zip (orthogonal preference) (repeat $ (1-prob)/2)
--map (\x -> (x,(1 - prob)/2)) (orthogonal preference)

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
	randomPos <- fromList $ zip (mGetAllIndex lab) (repeat 1)
	--let lab' = lab
	lab' <- boreTunnel randomPos Right lab
	lab'' <- boreTunnel (randomPos <+> (-1,0)) Left lab'
	--let lab'' = lab'
	randomTunnels lab'' wallRatio
	where
		currentWallRatio = (fromIntegral countWall) / (fromIntegral $ width*height)
		countWall = F.foldl (+) 0 (fmap toInt lab)
		(width,height) = (mGetWidth lab, mGetHeight lab)
		toInt Wall = 1
		toInt Free = 0

boreTunnel pos0 favDir matr = calcNewMatr (wormBehaviour (favDir,0.9)) pos0 matr

inBox :: Area -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox <+> sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
