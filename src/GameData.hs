module GameData where

import Vector2D
import Graphics.Gloss hiding(Point)
import Math.Matrix

type Pos = Vec Int -- probably deprecated
type PosF = Vec Float
type Direction = Vec Int -- probably deprecated
type DirectionF = Vec Float
type Size = Vec Int
type Box = (Pos,Size)

type Time = Float

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
data Territory = Free | Wall deriving(Show)
data Items = Items {
	dots :: Object,
	fruits :: Object
}

data Characters = Characters {
	pacMan :: MovableObj,
	monsters :: MovableObj
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
	speed :: DirectionF
}

{-data ObjectType = Dot | Fruit
data MovableObjType = PacMan | Monster-}
