module HsPacMan where

import Graphics.Gloss hiding(Point)

type Point = Vec Int
type Direction = Vec Int

type Vec a = (a,a)


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

data Labyrinth = Labyrinth
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
	pos :: Point
}

data RenderParams = RenderParams {
	pic :: Picture
}

data MovableObj = MovableObj {
	obj :: Object,
	movableParams :: MovableParams
}

data MovableParams = MovableParams {
	speed :: Direction
}

data ObjectType = Dot | Fruit
data MovableObjType = PacMan | Monster
