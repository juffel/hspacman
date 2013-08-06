module Renderpipeline where

import GameData
import LevelGenerator
import Vector2D
import Math.Matrix
import qualified Data.Foldable as F -- enables folds over matrices
import Data.Tuple

import Graphics.Gloss hiding(display)
import qualified Graphics.Gloss as G

-- used to represent coordinates relative to an area on the screen:
-- (0,0)..(1,1)
type NormalCoords = Vec Float
type NormalSize = Vec Float


-- origin is the upper left corner of the screen (x-Axis right, y-Axis dowdown
type PosOnScreen = Vec Float
--  (0,0) .. (screenWidth,screenHeight)	
-- x right, y down

type SizeOnScreen = Vec Float
type AreaOnScreen = (PosOnScreen,SizeOnScreen)

-- nice aliases:
type WindowSize = SizeOnScreen
type DestAreaOnScreen = AreaOnScreen


-- origin is the center of the screen!, (x-Axis right, y-Axis up)
type GlossCoords = Vec Float


renderWorld :: WindowSize -> World -> Picture
renderWorld wSize world = case (uiState world) of
	Menu -> renderMenu wSize menuArea world
	Playing -> renderGame wSize dbgTextArea gameArea world
	where
		gameArea = ((textAreaWidth,0), wSize <-> (textAreaWidth,0))
		dbgTextArea = ((0,0),    (textAreaWidth,vecY wSize))
		menuArea = ((0,0),wSize)
		textAreaWidth = 200

renderMenu :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderMenu wSize destArea world = Color yellow $ Polygon $
	map (normalizedPosToGloss wSize destArea) $ rect (0,0) (1,1)

renderGame :: WindowSize -> DestAreaOnScreen -> DestAreaOnScreen -> World -> Picture
renderGame wSize dbgTextArea gameArea world = Pictures $ [
	renderGameArea wSize gameArea world ,
	renderDbgText wSize dbgTextArea world
	]

renderGameArea :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderGameArea wSize destArea world = Pictures [
	Color white $ Line (fmap (normalizedPosToGloss wSize destArea) $ rect (0,0) (1,1)),
	renderLabyrinth wSize destArea cellSize (labyrinth world),
	renderPacMan wSize destArea cellSize (pacman world),
	renderGhosts wSize destArea cellSize (ghosts world) ]
	where
		cellSize :: SizeF
		cellSize = (1,1) </> (fromIntegral $ mGetWidth lab, fromIntegral $ mGetHeight lab)
		lab = labyrinth world 


renderPacMan :: WindowSize -> DestAreaOnScreen -> SizeF -> Pacman -> Picture
renderPacMan wSize gameArea@(fieldPos,fieldSize) cellSize pacman =
	renderChar wSize gameArea cellSize pacman $
	Translate (1/2) (-1/2) $
	Color yellow $
	--Rotate 90 $ -- roation doesn't work due to a bug in gloss!!
	ThickArc (mouthAngle/2) (-mouthAngle/2) (1/4) (1/2)
	where
		mouthAngle = 90 * (sin $ 5 * t pacman)

renderGhosts :: WindowSize -> DestAreaOnScreen -> SizeF -> [Ghost] -> Picture
renderGhosts wSize gameArea@(fieldPos,fieldSize) cellSize ghosts = Pictures $ map (renderGhost wSize gameArea cellSize) ghosts
	
	
renderGhost :: WindowSize -> DestAreaOnScreen -> SizeF -> Ghost -> Picture
renderGhost wSize gameArea@(fieldPos,fieldSize) cellSize ghost = renderChar wSize gameArea cellSize ghost$
	Translate (0) (-1) $
	--Translate (1/2) (-1/2) $
	Color green $
	(Polygon $ rect (0,0) (1,1))
	where
		mouthAngle = 90 * (sin $ 5 * t ghost)


renderChar :: WindowSize -> DestAreaOnScreen -> SizeF -> Object st -> Picture -> Picture
renderChar wSize gameArea@(fieldPos,fieldSize) cellSize obj pic =
	(uncurry Translate) (normalizedPosToGloss wSize gameArea (cellSize <*> (pos obj))) $
	(uncurry Scale) (size obj) $
	(uncurry Scale) (normalizedPosToScreen gameArea cellSize <-> normalizedPosToScreen gameArea (0,0)) $
	pic
	{-Translate (1/2) (-1/2) $
	Color yellow $
	--Rotate 90 $ -- roation doesn't work due to a bug in gloss!!
	ThickArc (mouthAngle/2) (-mouthAngle/2) (1/4) (1/2)
	where
		mouthAngle = 90 * (sin $ 5 * t obj)-}




renderLabyrinth :: WindowSize -> DestAreaOnScreen -> SizeF -> Labyrinth -> Picture
renderLabyrinth wSize destArea cellSize lab = Pictures $ F.foldr (:)[] $ mapWithIndex drawCell lab
	where
        -- in order to display the matrix correctly the lines/columns have to be flipped when drawing
		drawCell :: MatrIndex -> Territory -> Picture
		drawCell coords = drawCell' (swap coords)
		drawCell' coords ter = case ter of
			Free -> Color (greyN 0.8) $ Polygon $ rect posCell sizeCell
			Wall -> Color (greyN 0.2) $ Polygon $ rect posCell sizeCell
	    		where
				posCell = posFromCoords coords
				sizeCell= posFromCoords (coords <+> (1,1)) <-> posCell
				posFromCoords coords = normalizedPosToGloss wSize destArea $ fOnVec fromIntegral coords <*> cellSize


rect :: NormalCoords -> NormalSize -> [NormalCoords]
rect pos (w,h) = [ pos, pos<+>(0,h), pos<+>(w,h), pos<+>(w,0), pos ]



renderDbgText :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderDbgText wSize destArea world = renderLines 0 $ lines $ info $ dbgInfo world
	where
		renderLines :: Float -> [String] -> Picture
		renderLines yPos lines = case lines of
			[] -> Blank
			(x:xs) -> Pictures $ [
				(uncurry Translate) (normalizedPosToGloss wSize destArea (0,0.1+yPos)) $
				Color white $ 
				Scale 0.1 0.1 $
				Text x,
				renderLines (yPos+lineHeight) xs ]
			where lineHeight = min 
				((1-yPos) / (fromIntegral $ length lines))
				0.1

-- coordinate translaters:

normalizedPosToGloss :: WindowSize -> DestAreaOnScreen -> NormalCoords -> GlossCoords
normalizedPosToGloss wSize destArea pos = toGloss wSize $ normalizedPosToScreen destArea pos

normalizedPosToScreen :: DestAreaOnScreen -> NormalCoords -> PosOnScreen
normalizedPosToScreen (posOnScr,sizeOnScr) pos = posOnScr <+> pos <*> sizeOnScr

{- game programmers are used to (0,0) to be in the left upper corner of the screen,
-- and (ScreenWidth,ScreenHeight) to be the right bottom corner.
-- GLOSS uses ccoords whose origin is the center of the screen,
-- x-Axis and y-Axis pointint right/up (like in math)
-}
toGloss :: WindowSize -> PosOnScreen -> GlossCoords
toGloss wSize pos = (pos <-> (wSize </ 2)) <*> (1,-1)

fromGloss :: WindowSize -> GlossCoords -> PosOnScreen
fromGloss wSize pos = pos <*> (1,-1) <+> (wSize </2)
