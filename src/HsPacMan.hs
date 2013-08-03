module Main where

import GameData
import LevelGenerator
import Vector2D

import Graphics.Gloss hiding(display)
import qualified Graphics.Gloss as G

windowTitle = "hsPacMan"
windowPos = (100, 100) :: Size
windowSize = (800, 600) :: Size

main = play
	display
	bgColour
	framerate
	startWorld
	renderWorld
	handleInput
	moveWorld

display = InWindow windowTitle windowSize windowPos
bgColour = black
framerate = 40

startWorld = undefined

renderWorld :: World -> Picture
renderWorld world = case (uiState $ settings world) of -- check wether the game is currently in status Playing or Menu
    Playing ->  (uncurry Translate) (i2C (fromIntegral 0, fromIntegral 0)) $ -- Translation
                Scale 1 (-1) $ -- flip y-axis
                renderPlaying world

    Menu -> renderMenu world

renderPlaying :: World -> Picture
renderPlaying = undefined

renderMenu :: World -> Picture
renderMenu _ = Pictures [ Text "Pacman - The Menu", renderBg windowSize ]

renderBg :: Size -> Picture
renderBg (sX, sY) = Color black $ Polygon path
    where
            sizeX = fromIntegral sX
            sizeY = fromIntegral sY
            path = [ (0,0), (sizeX, 0), (sizeX, sizeY), (0, sizeY), (0,0) ]

handleInput = undefined
moveWorld = undefined

-- ingame top-left-zero coordinates to gloss centered-zero coordinates
i2C :: Vec Float -> Vec Float
i2C (x, y) = (transfX, transfY)
    where   transfX = x -(fromIntegral $ fst windowSize)/2 -- simple translation from [0, width] -> [-width/2, +width/2]
            transfY = -(y -(fromIntegral $ snd windowSize)/2) -- translation plus flip from [0, height] -> [+height/2, -height/2]
