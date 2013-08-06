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

import Data.Tuple
import Data.List

windowTitle = "hsPacMan"
windowPos = (100, 100)  :: PosOnScreen
windowSize = (800, 600) :: SizeOnScreen

main = play
	display
	bgColour
	framerate
	(startWorld 2)
	(renderWorld windowSize) -- calls renderWorld from Module Renderpipeline
	handleInput
	moveWorld

display = InWindow windowTitle (fOnVec floor windowSize) (fOnVec floor windowPos)
bgColour = black
framerate = 60

startWorld seed = World {
    uiState=Menu,
    level=1,
    points=0,
    labyrinth=genLabyrinth (30,20) 0.5 seed,
    pacman=Object{pos=(1, 1), size=pacManSize, direction=(0,0), t=0 },
    ghosts=undefined,
    dots=undefined,
    fruits=undefined,
    dbgInfo = DbgInf{ info = "test\ntest" }
}
	where
		pacManSize = (0.7,0.7)

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
                    Char 'w' -> setPacDir world (directionToSpeed GameData.Up)
                    Char 's' -> setPacDir world (directionToSpeed GameData.Down)
                    Char 'a' -> setPacDir world (directionToSpeed GameData.Left)
                    Char 'd' -> setPacDir world (directionToSpeed GameData.Right)
                    SpecialKey KeySpace -> setPacDir world (0,0)
                    _ -> world --alternative playing

    _ -> world -- ignore other events

setUIState :: World -> UIState -> World
setUIState world state = world {uiState = state}

-- |changes the moving direction of the pacman
setPacDir :: World -> SpeedF -> World
setPacDir world dir = world {pacman = (pacman world) {direction=dir}}

moveWorld :: DeltaT -> World -> World
moveWorld deltaT world = (movePacman deltaT) $ (moveGhosts deltaT) world

moveGhosts :: DeltaT -> World -> World
moveGhosts d world = world

movePacman :: DeltaT -> World -> World
movePacman d world@World{ pacman=pacMan } =
	world { pacman=pacMan{ pos=newPos, t=(t pacMan + d) }, dbgInfo=DbgInf{ info=dbgText} }
	where
		--newPos = pos pacMan <+> direction pacMan <* d
		dbgText =
			"pos pacMan: " ++ (show $ fOnVec floor $ pos pacMan) ++ "\n" ++
			"pos pacMan exact: " ++ (show $ pos pacMan) ++ "\n" {-++
			"possibleDirs: " ++ show possibleDirs-}
		newPos = if (willCollide (labyrinth world) speed d pacMan)
			then pos pacMan
			else (pointInSizeF labSize $ pos pacMan <+> (direction pacMan) <* (speed * d)) -- pointInSize: torus
				where
					labSize = fOnVec fromIntegral (mGetWidth lab -1,mGetHeight lab -1)
					lab = labyrinth world
		speed = 2


willCollide lab speed deltaT obj = or $ map (willPointCollide lab speed deltaT (direction obj)) $
	[ p , p <+> (0,h), p <+> (w,h), p <+> (w,0) ]
	where
		p = pos obj
		(w,h) = size obj

willPointCollide lab speed deltaT dir oldPos = (==Wall) $ mGet (calcMatrIndex nextPos) lab 
	where
		calcMatrIndex :: PosF -> MatrIndex
		calcMatrIndex nextPos = swap $
			fOnVec floor $
			pointInSizeF (fromIntegral $ mGetWidth lab -1, fromIntegral $ mGetHeight lab -1) nextPos -- torus
		nextPos = (oldPos <+> dir <* (speed * deltaT))

{-possibleDirections :: Labyrinth -> Object -> [Direction]
possibleDirections lab obj = filter (objCanMoveThere lab obj) allDirs

objCanMoveThere :: Labyrinth -> Object -> Direction -> Bool
objCanMoveThere lab obj@Object{ pos=pos, size=size } dir =
	--foldl (&&) True $ 
	and $
	map ((==Free) . directionToTerritory lab dir . fOnVec floor) [pos] --[pos,pos+size]

-- |used to check wether it is possible for a moving object to proceed moving in the current direction
directionToTerritory :: Labyrinth -> Direction -> Pos -> Territory
directionToTerritory lab dir pos = mGet (swap newPos) lab
	where
		newPos = movePoint (mGetWidth lab,mGetHeight lab) pos (directionToSpeed dir)
		-}
