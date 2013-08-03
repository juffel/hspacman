module Main where

import GameData
import LevelGenerator
import Renderpipeline
import Vector2D
import Math.Matrix

import Graphics.Gloss hiding(display)
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss as G

windowTitle = "hsPacMan"
windowPos = (100, 100)  :: PosOnScreen
windowSize = (800, 600) :: SizeOnScreen

fieldArea = ((0,0),(800,600)) :: AreaOnScreen

main = play
	display
	bgColour
	framerate
	startWorld
	(renderWorld fieldArea) -- calls renderWorld from Module Renderpipeline
	handleInput
	moveWorld

display = InWindow windowTitle (fOnVec floor windowSize) (fOnVec floor windowPos)
bgColour = black
framerate = 40

startWorld = World {
    settings = Settings {
                    uiState=Menu,
                    gameState=GameState {level=1,points=1000} },
    game = GameData {
                labyrinth=undefined,
                items=undefined,
                characters=undefined}
}

handleInput :: Event -> World -> World
handleInput event = id 

moveWorld :: DeltaT-> World -> World
moveWorld deltaT = id 
