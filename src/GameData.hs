module GameData where

import Vector2D
import Graphics.Gloss hiding(Point)
import Math.Matrix

import Prelude hiding(Left,Right)

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
    level :: Level,
    points :: Points,
    labyrinth :: Labyrinth,
    pacman :: Pacman,
    ghosts :: [Ghost],
    dots :: [Dot],
    fruits :: [Fruit]
}

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

data Object = Object {
    pos :: PosF,
    size :: SizeF,
    --speed :: Float , 
    direction :: SpeedF
    --direction :: Direction
}

type Dot = Object
type Fruit = Object
type Pacman = Object
type Ghost = Object

data UIState = Playing | Menu

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
				

-- realizes a "torus like" behavior for positions on the field
getNeighbourIndex :: Size -> MatrIndex -> Movement -> MatrIndex
getNeighbourIndex (width,height) pos@(x,y) dir = case dir of
	Up -> (x,(y-1) `niceMod` height)
	--UpRight -> getNeighbourIndex field (getNeighbourIndex field pos Up) Right
	Right -> ((x+1) `niceMod` width, y)
	--DownRight -> getNeighbourIndex field (getNeighbourIndex field pos Down) Right
	Down -> (x,(y+1) `niceMod` height)
	--DownLeft -> getNeighbourIndex field (getNeighbourIndex field pos Down) Left
	Left -> ((x-1) `niceMod` width, y)
--UpLeft -> getNeighbourIndex field (getNeighbourIndex field pos Up) Left
	where
		{-width = mGetWidth field
		height = mGetHeight field-}
		niceMod val m = case signum val of
			(-1) -> niceMod (val+m) m
			(1) -> val `mod` m
			(0) -> 0
			otherwise -> error "niceMod internal error!"

{-data ObjectType = Dot | Fruit
data MovableObjType = PacMan | Monster-}
