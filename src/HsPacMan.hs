module Main where

import GameData
import LevelGenerator
import Renderpipeline
import Vector2D
import Math.Matrix
import RandomUtils

import Prelude hiding(Left,Right)

import Prelude hiding(Left,Right)

import Graphics.Gloss hiding(display)
import Graphics.Gloss.Interface.Pure.Game hiding(Up,Down)
import qualified Graphics.Gloss.Interface.Pure.Game as G
import qualified Graphics.Gloss as G

import System.Random
import Control.Monad.Random

import Data.Tuple
import Data.List

windowTitle = "hsPacMan"
windowPos = (100, 100)  :: PosOnScreen
windowSize = (800, 600) :: SizeOnScreen

main = play
	display
	bgColour
	framerate
	(startWorld 0)
	(renderWorld windowSize) -- calls renderWorld from Module Renderpipeline
	handleInput
	moveWorld

display = InWindow windowTitle (fOnVec floor windowSize) (fOnVec floor windowPos)
bgColour = black
framerate = 40

startWorld seed = World {
    uiState=Menu,
    level=1,
    points=0,
    labyrinth=genLabyrinth (40,30) 0.4 seed,
    pacman=Object{pos=(1, 1), size=pacManSize, direction=(0,0), t=0, state=() },
    ghosts=ghosts,
    dots=undefined,
    fruits=undefined,
    dbgInfo = DbgInf{ info = "test\ntest" },
    keys = []
}
	where
		pacManSize = (0.7,0.7)
		ghosts = [
			Object{ pos=(0,0), size=pacManSize, direction=(0,0), t=0, state=GhostState { rndState= mkStdGen seed } }]

handleInput :: Event -> World -> World
handleInput event world = case event of
	(EventKey key upOrDown _ _) -> case (uiState world) of
		Menu -> case upOrDown of
		  	G.Down -> case key of
			-- Offnen: Menu hat entweder Punkte die durch einen Cursor ausgewählt werden
			-- oder: Menu hat Optionen die durch bestimmte Tasten ausgelöst werden.
				{-SpecialKey KeyEnter -> undefined    -- menuepunkt auswählen
				SpecialKey KeyUp -> undefined       -- einen menupunkt hoeher
				SpecialKey KeyDown -> undefined     -- einen menupunkt tiefer
				SpecialKey KeyEsc -> undefined    -- spiel verlassen-}
				Char 's' -> setUIState (startWorld 8) Playing
				Char 'p' -> undefined -- TODO: pause
				_ -> world --alternative menue
			_ -> world --alternative menue
		Playing -> case upOrDown of
			G.Down -> case key of
				Char 'w' -> world{ keys= addDir Up }
				Char 's' -> world{ keys= addDir Down }
				Char 'a' -> world{ keys= addDir Left }
				Char 'd' -> world{ keys= addDir Right }
				SpecialKey KeySpace -> setPacDir world (0,0)
				--_ -> world --alternative playing
			G.Up -> case key of
				Char 'w' -> world{ keys= remDir Up }
				Char 's' -> world{ keys= remDir Down }
				Char 'a' -> world{ keys= remDir Left }
				Char 'd' -> world{ keys= remDir Right }
				SpecialKey KeySpace -> setPacDir world (0,0)
		where
			addDir dir = [dir] `union` (remDir $ opposite dir)
			remDir dir = filter (/=dir) currentKeys
			currentKeys = keys world
	_ -> world --alternative playing


setUIState :: World -> UIState -> World
setUIState world state = world {uiState = state}

-- |changes the moving direction of the pacman
setPacDir :: World -> SpeedF -> World
setPacDir world dir = world {pacman = (pacman world) {direction=dir}}

moveWorld :: DeltaT -> World -> World
moveWorld deltaT world = (movePacman deltaT) $ (moveGhosts deltaT) world

moveGhosts :: DeltaT -> World -> World
moveGhosts dt world =
	world{
		ghosts= map (moveCharacter dt world . setDirection world) (ghosts world),
		dbgInfo=DbgInf{ info= show $ possibleDirections (labyrinth world) dt (head $ ghosts world ) }
	}
	where
		--monsterSpeed = (fromIntegral $ level world)
		-- this function is the ai for the ghosts
		setDirection :: World -> Ghost -> Ghost
		setDirection world ghost = ghost{
			direction= direction $ pacman world,
			--direction= fOnVec fromIntegral $ directionsToSpeed [newDir],
			state = state ghost
			--state = GhostState{ rndState=newRndState }
		}
			where
				(newDir,newRndState) = runRand rndDir (rndState $ state ghost)
				rndDir = randomDirS possibleDirs [(head $ speedToDirection $ direction ghost,0.98)]
				possibleDirs = possibleDirections (labyrinth world) dt ghost
				pacManObj= pacman world


movePacman :: DeltaT -> World -> World
movePacman dt world@World{ pacman=pacMan } =
	world {
		pacman = moveCharacter dt world $ setDirection pacMan
		--,
		--dbgInfo=DbgInf{ info=dbgText} }
	}
	where
		setDirection :: Pacman -> Pacman
		setDirection obj = obj{ direction = speed *> (fOnVec fromIntegral $ directionsToSpeed $ keys world) }
		{-dbgText =
			"pos pacMan: " ++ (show $ fOnVec floor $ pos pacMan) ++ "\n" ++
			"pos pacMan exact: " ++ (show $ pos pacMan) ++ "\n" {-++
			"possibleDirs: " ++ show possibleDirs-} -}
		speed = 2

moveCharacter :: DeltaT -> World -> Object st -> Object st
moveCharacter dt world obj = obj{ pos=newPos, t= (t obj + dt) }
	where
		
		newPos = if (willCollide (labyrinth world) dt obj)
			then pos obj
			else (pointInSizeF labSize $ pos obj <+> (direction obj) <* dt) -- pointInSize: torus
				where
					labSize = fOnVec fromIntegral (mGetWidth lab -1,mGetHeight lab -1)
					lab = labyrinth world

possibleDirections :: Labyrinth -> DeltaT -> Object st -> [Direction]
possibleDirections lab deltaT obj = filter (not . willCollide lab deltaT . (\x -> obj{ direction= x }) . directionToSpeed) $
	allDirs

willCollide lab deltaT obj = or $ map (willPointCollide lab deltaT (direction obj)) $
	[ p , p <+> (0,h), p <+> (w,h), p <+> (w,0) ]
	where
		p = pos obj
		(w,h) = size obj

willPointCollide lab deltaT dir oldPos = (==Wall) $ mGet (calcMatrIndex nextPos) lab 
	where
		calcMatrIndex :: PosF -> MatrIndex
		calcMatrIndex nextPos = swap $
			fOnVec floor $
			pointInSizeF (fromIntegral $ mGetWidth lab -1, fromIntegral $ mGetHeight lab -1) nextPos -- torus
		nextPos = (oldPos <+> dir <* deltaT)

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
