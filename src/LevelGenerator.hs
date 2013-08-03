module LevelGenerator where

import GameData
import Vector2D

import Math.Matrix
import Control.Monad.State

data Direction = Up | Down | Right | Left

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

-- Post -> (Maybe Movement, Pos)
--type PosState a = State (Matrix a, Pos) (a->a, Maybe Movement)

--squareCloseToTheEdge f border = state $ \(matr,pos) ->
	--(f (mGet pos), 


{-right :: PosState
right = state $ \pos0 -> (newDir,pos <+> (1,0))
	where
		newDir = if vecX pos0 < maxPos then Just Right else Nothing-}

{-mapWithPath :: [PosState ((a->a),Movement) ] -> Matrix a -> Matrix a
mapWithPath (x:xs) matr = -}

--type Behaviour = (View -> (a,Maybe Direction)) 

--calcNewMatr :: Behaviour -> Matrix -> Matrix a -> Matrix a

genLabyrinth :: Size -> Int -> Labyrinth
genLabyrinth (width,height) seed = 
	circle $
	mUnsafe (take height $ repeat lines)
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
		lines = take width $ repeat Wall


inBox :: Box -> Pos -> Bool
inBox (posBox,sizeBox) pos = (pos `vecGOE` posBox) && (pos `vecSOE` (posBox <+> sizeBox))
	where
		vecGOE l r = (vecX l >= vecX r) && (vecY l >= vecY r)
		vecSOE l r = (vecX l <= vecX r) && (vecY l <= vecY r)
