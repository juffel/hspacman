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
type Speed = Vec Int -- | movement vector
type SpeedF = Vec Float
type Size = Vec Int
type SizeF = Vec Float
type Area = (Pos,Size)

type Time = Float
type DeltaT = Float

data World = World {
	settings :: Settings,
	game :: GameData
}

data Settings = Settings {
	uiState :: UIState,
	gameState :: GameState
}

data UIState = Playing | Menu

data GameState = GameState {
	level :: Level,
	points :: Points
}

type Level = Int
type Points = Int

data GameData = GameData {
	labyrinth :: Labyrinth,
	items :: Items,
	characters :: Characters
}

type Labyrinth = Matrix Territory
data Territory = Free | Wall deriving(Show,Eq)
data Items = Items {
	dots :: Object,
	fruits :: Object
}

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
}


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
