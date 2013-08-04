module Main where

import GameData
import LevelGenerator
import Renderpipeline
import Vector2D
import Math.Matrix

import Graphics.Gloss hiding(display)
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Interface.Pure.Game as G
import qualified Graphics.Gloss as G

windowTitle = "hsPacMan"
windowPos = (100, 100)  :: PosOnScreen
windowSize = (800, 600) :: SizeOnScreen

fieldArea = ((0,0),(800,600)) :: AreaOnScreen
{-testLab :: Labyrinth
testLab = mUnsafe [ [Wall, Wall, Free, Wall],
                    [Free, Wall, Free, Wall],
                    [Free, Free, Wall, Wall] ]
		    -}

main = play
	display
	bgColour
	framerate
	(startWorld 2)
	(renderWorld fieldArea) -- calls renderWorld from Module Renderpipeline
	handleInput
	moveWorld

display = InWindow windowTitle (fOnVec floor windowSize) (fOnVec floor windowPos)
bgColour = black
framerate = 60

startWorld seed = World {
    uiState=Menu,
    level=1,
    points=0,
    labyrinth=genLabyrinth (30,40) 0.5 seed,
    pacman=Object{pos=(2.5, 5.5), speed=5, direction=GameData.Right},
    ghosts=undefined,
    dots=undefined,
    fruits=undefined
}

handleInput :: Event -> World -> World
handleInput event world =
    case event of
    (EventKey key G.Down _ _) ->
            case (uiState world) of
                Menu -> case key of
                -- Offnen: Menu hat entweder Punkte die durch einen Cursor ausgewählt werden
                -- oder: Menu hat Optionen die durch bestimmte Tasten ausgelöst werden.
                    {-SpecialKey KeyEnter -> undefined    -- menuepunkt auswählen
                    SpecialKey KeyUp -> undefined       -- einen menupunkt hoeher
                    SpecialKey KeyDown -> undefined     -- einen menupunkt tiefer
                    SpecialKey KeyEsc -> undefined    -- spiel verlassen-}
                    Char 's' -> setUIState (startWorld 8) Playing
                    Char 'p' -> undefined -- TODO: pause
                    _ -> world --alternative menue
                Playing -> case key of
                    Char 'w' -> setPacDir world GameData.Up
                    Char 's' -> setPacDir world GameData.Down
                    Char 'a' -> setPacDir world GameData.Left
                    Char 'd' -> setPacDir world GameData.Right
                    _ -> world --alternative playing

    _ -> world -- ignore other events

setUIState :: World -> UIState -> World
setUIState world state = world {uiState = state}

-- |changes the moving direction of the pacman
setPacDir :: World -> Direction -> World
{-setPacDir world dir = case dir of
    GameData.Up -> world {pacman = (pacman world) {dirSpeed= (0, -abs)}}
    GameData.Down -> world {pacman = (pacman world) {dirSpeed= (0, abs)}}
    GameData.Left -> world {pacman = (pacman world) {dirSpeed= (-abs, 0)}}
    GameData.Right -> world {pacman = (pacman world) {dirSpeed= (abs, 0)}}
    where
        abs = vecLength (dirSpeed $ pacman $ world) -}
setPacDir world dir = world {pacman = (pacman world) {direction=dir}}

moveWorld :: DeltaT -> World -> World
moveWorld deltaT world = manageCollisions $ (movePacman deltaT) $ (moveGhosts deltaT) world
-- move pacman
-- move ghosts
-- check for collisions/item pickups

-- TODO
moveGhosts :: DeltaT -> World -> World
moveGhosts d world = world

movePacman :: DeltaT -> World -> World
movePacman d world = world {pacman= (pacman world) {pos=newPos}}
    where
        newPos = (pos $ pacman $ world) <+> (d *> dirSpeed)
        dirSpeed = case (direction $ pacman $ world) of
            GameData.Up -> (0, -(speed $ pacman $ world))
            GameData.Down -> (0, (speed $ pacman $ world))
            GameData.Right-> ((speed $ pacman $ world), 0)
            GameData.Left -> (-(speed $ pacman $ world), 0)

-- |used to check wether it is possible for a moving object to proceed moving in the current direction
directionFree :: Labyrinth -> Pos -> Direction -> Bool
directionFree lab pos dir = True

-- TODO
manageCollisions :: World -> World
manageCollisions world = world
