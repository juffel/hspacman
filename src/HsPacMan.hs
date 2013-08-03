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


startWorld = undefined
renderWorld = undefined
handleInput = undefined
moveWorld = undefined

