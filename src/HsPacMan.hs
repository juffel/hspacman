module Main where

import GameData hiding(Down)
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
	(startWorld 8)
	--(\x -> Pictures [])
	(renderWorld fieldArea) -- calls renderWorld from Module Renderpipeline
	handleInput
	moveWorld

display = InWindow windowTitle (fOnVec floor windowSize) (fOnVec floor windowPos)
bgColour = black
framerate = 40

startWorld seed = World {
    settings = Settings {
                    uiState=Menu,
                    gameState=GameState {level=1,points=0} },
    game = GameData {
        labyrinth=genLabyrinth (30,29) 0.95 seed,
        items=undefined,
        characters=Characters {
            pacMan=MovableObj{
                obj=Object{
                    objParams=ObjParams{pos=(5,5)},
                    renderParams=RenderParams{pic=undefined}
                },
                movableParams=MovableParams{speed=1}
            },
            monsters=[]
        }
    }
}

handleInput :: Event -> World -> World
handleInput event world =
    case event of
    (EventKey key Down _ _) ->
            case uiState (settings world) of
                Menu -> case key of
                -- Offnen: Menu hat entweder Punkte die durch einen Cursor ausgewählt werden
                -- oder: Menu hat Optionen die durch bestimmte Tasten ausgelöst werden.
                    {-SpecialKey KeyEnter -> undefined    -- menuepunkt auswählen
                    SpecialKey KeyUp -> undefined       -- einen menupunkt hoeher
                    SpecialKey KeyDown -> undefined     -- einen menupunkt tiefer
                    SpecialKey KeyEsc -> undefined    -- spiel verlassen-}
                    Char 's' -> setUIState (startWorld 8) Playing
                    _ -> world --alternative menue
                Playing -> case key of
                    Char 'w' -> undefined -- pacman hoch laufen lassen
                    Char 's' -> undefined -- pacman runter laufen lassen
                    Char 'a' -> undefined -- pacman nach links laufen lassen
                    Char 'd' -> undefined -- pacmann nach rechts laufen lassen
                    _ -> world --alternative playing

    _ -> world -- ignore other events

-- |dont look into implementation! it could kill you with its ugliness
-- (blame the record syntax!!!)
setUIState :: World -> UIState -> World
setUIState world state = world{ settings= (settings world){ uiState=state } }

moveWorld :: DeltaT-> World -> World
moveWorld deltaT = id 
