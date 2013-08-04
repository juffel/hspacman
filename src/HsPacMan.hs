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
	(startWorld 99)
	--(\x -> Pictures [])
	(renderWorld fieldArea) -- calls renderWorld from Module Renderpipeline
	handleInput
	moveWorld

display = InWindow windowTitle (fOnVec floor windowSize) (fOnVec floor windowPos)
bgColour = black
framerate = 40

startWorld seed = World {
    uiState=Menu,
    level=1,
    points=0,
    labyrinth=genLabyrinth (30,29) 0.95 seed,
    pacman=Object{pos=(2.5, 5.5), dirSpeed=(1,0)},
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
                    Char 'w' -> movePac GameData.Up world
                    Char 's' -> movePac GameData.Down world
                    Char 'a' -> movePac GameData.Left world
                    Char 'd' -> movePac GameData.Right world
                    _ -> world --alternative playing

    _ -> world -- ignore other events

setUIState :: World -> UIState -> World
setUIState world state = world {uiState = state}

moveWorld :: DeltaT -> World -> World
moveWorld deltaT world = manageCollisions $ (movePacman deltaT) $ (moveGhosts deltaT) world
-- move pacman
-- move ghosts
-- check for collisions/item pickups

-- TODO
moveGhosts :: DeltaT -> World -> World
moveGhosts d world = world

movePacman :: DeltaT -> World -> World
movePacman d world = world

-- TODO
manageCollisions :: World -> World
manageCollisions world = world

-- kollision bedenken?
movePac :: Movement -> World -> World
movePac mov world = case mov of
     GameData.Up -> undefined
     GameData.Down -> undefined
     GameData.Left -> undefined
     GameData.Right -> undefined
