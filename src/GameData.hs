module GameData where

import Vector2D
import Graphics.Gloss hiding(Point)
import Math.Matrix

import Prelude hiding(Left,Right)

-- |Directions in Labyrinth
data Direction = Up | Down | Right | Left deriving (Eq, Show)
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

data Object = Object {
    pos :: PosF,
    dirSpeed :: SpeedF  -- vector combining speed and direction
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

{-data ObjectType = Dot | Fruit
data MovableObjType = PacMan | Monster-}
