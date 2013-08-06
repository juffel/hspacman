module GameData where

import Vector2D
import Graphics.Gloss hiding(Point)
import Math.Matrix

import Prelude hiding(Left,Right)
import Data.Fixed
import Data.Typeable

-- |Directions in Labyrinth
data Direction = Up | Down | Right | Left deriving (Eq, Show)


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
-- |Movement on Labyrinth
type Movement = Direction

type Pos = Vec Int -- probably deprecated
type PosF = Vec Float -- logical Position on field
type Speed = Vec Int -- probably deprecated
type SpeedF = Vec Float -- movement vector
type Size = Vec Int
type SizeF = Vec Float
type Area = (Pos,Size)

type Time = Float
type DeltaT = Float

data World = World {
	uiState :: UIState,
	keys :: CurrentKeys,
	level :: Level,
	points :: Points,
	labyrinth :: Labyrinth,
	pacman :: Pacman,
	ghosts :: [Ghost],
	dots :: [Dot],
	fruits :: [Fruit],
	dbgInfo :: DebugInfo
} deriving(Show)

type CurrentKeys = [Direction]

data DebugInfo = DbgInf {
	info :: String
} deriving(Show)

type Level = Int
type Points = Int

type Labyrinth = Matrix Territory
data Territory = Free | Wall deriving(Show,Eq)
instance Enum Territory where
	toEnum int = case int of
		0 -> Free
		1 -> Wall
	fromEnum ter = case ter of
		Free -> 0
		Wall -> 1

data Object objState = Object {
	pos :: PosF,
	size :: SizeF,
	--speed :: Float , 
	direction :: SpeedF,
	t :: Time,
	state :: objState
	--direction :: Direction
} deriving(Show)

data GhostState = GhostState {

} deriving(Show)

type Dot = Object ()
type Fruit = Object ()
type Pacman = Object ()
type Ghost = Object GhostState

data UIState = Playing | Menu deriving(Show)

{-
data World = World {
	settings :: Settings,
	game :: GameData
}

data Settings = Settings {
	uiState :: UIState,
	gameState :: GameState
}


data GameState = GameState {
	level :: Level,
	points :: Points
} -}

{-
data GameData = GameData {
	labyrinth :: Labyrinth,
	items :: Items,
	characters :: Characters
} -}


{- data Items = Items {
	dots :: Object,
	fruits :: Object
} -}

{-
data Characters = Characters {
	pacMan :: MovableObj,
	monsters :: [MovableObj]
}

data Object = Object {
	objParams :: ObjParams,
	renderParams :: RenderParams
}

data ObjParams = ObjParams {
	pos :: PosF
}

data RenderParams = RenderParams {
	pic :: Picture
}

data MovableObj = MovableObj {
	obj :: Object,
	movableParams :: MovableParams
} 

data MovableParams = MovableParams {
	speed :: SpeedF
} -}
directionsToSpeed = foldl (<+>) (0,0) . map directionToSpeed

directionToSpeed Up = (0,-1)
directionToSpeed Down = (0,1)
directionToSpeed Left = (-1,0)
directionToSpeed Right = (1,0)

speedToDirection :: SpeedF -> [Direction]
speedToDirection speed = 
	let 
		xDir = case (signum $ vecX speed) of
			1 -> [Right]
			(-1) -> [Left]
			0 -> []
		yDir = case (signum $ vecY speed) of
			1 -> [Down]
			(-1) -> [Up]
			0 -> []
	in
		xDir ++ yDir

{-rest :: (Num a, Integral m) => a -> m -> a
rest val m = case (cast val :: Maybe Int) of
	Just i -> i `mod` m
	_ -> case (cast val :: (Real a)=> a) of
		Just f -> f mod' m
		_ -> error "type not found!"
-}
	
{-
rest :: (Integral a) => a -> a -> a
rest = mod

restF :: (Real f) => f -> f -> f
restF = mod'
-}

pointInSize (width,height) (x,y)  = (x `mod` width, y `mod` height)
--pointInSize size pos = fOnVec (mod size) pos
--pointInSizeF size pos = fOnVec (mod' size) pos
pointInSizeF (width,height) (x,y)  = (x `mod'` width, y `mod'` height)

movePoint size pos dir = pointInSize size (pos <+> dir)
movePointF size pos dir = pointInSizeF size (pos <+> dir)

-- realizes a "torus like" behavior for positions on the field
{-getNeighbourIndex :: Size -> MatrIndex -> Movement -> MatrIndex
getNeighbourIndex (width,height) pos@(x,y) dir = case dir of
	Up -> (x,(y-1) `niceMod` height)
	Right -> ((x+1) `niceMod` width, y)
	Down -> (x,(y+1) `niceMod` height)
	Left -> ((x-1) `niceMod` width, y)
	where
		niceMod val m = case signum val of
			(-1) -> niceMod (val+m) m
			(1) -> val `mod` m
			(0) -> 0
			otherwise -> error "niceMod internal error!"
-}

{-data ObjectType = Dot | Fruit
data MovableObjType = PacMan | Monster-}
