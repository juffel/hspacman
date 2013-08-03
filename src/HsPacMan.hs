module Main where

import GameData

import Graphics.Gloss hiding(display)
import qualified Graphics.Gloss as G

progName = "hsPacMan"

windowTitle = progName
windowSize = (800,600)
windowPos = (10,10)


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

dispSettings = DisplaySettings {
    windowPos = (100, 100),
    windowSize = (400, 300)
}

startWorld = undefined

renderWorld :: World -> Picture
renderWorld world = case (uiState . setings world) of -- check wether the game is currently in status Playing or Menu
    Playing -> renderPlaying world
    Menu -> renderMenu world

renderPlaying :: World -> Picture
renderPlaying = undefined

renderMenu :: World -> Picture
renderMenu = Pictures [Text "Pacman - The Menu", renderBg  ]

renderBg :: DisplaySettings -> Picture
renderBg = undefined 

handleInput = undefined
moveWorld = undefined

-- ingame top-left-zero coordinates to gloss centered-zero coordinates
i2C :: Vec Float -> Vec Float
i2C (x, y) = (transfX, transfY)
    where   transpX = x -(fst windowSize)/2 -- simple translation from [0, width] -> [-width/2, +width/2]
            transpY = -(y -(snd windowSize)/2) -- translation plus flip from [0, height] -> [+height/2, -height/2]
