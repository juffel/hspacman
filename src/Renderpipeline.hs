module Renderpipeline where

import GameData
import LevelGenerator
import Vector2D
import Math.Matrix
import qualified Data.Foldable as F -- enables folds over matrices

import Graphics.Gloss hiding(display)
import qualified Graphics.Gloss as G

-- used to represent coordinates relative to an area on the screen:
-- (0,0)..(1,1)
type NormalCoords = Vec Float


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
		textAreaWidth = 100

renderMenu :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderMenu wSize destArea world = Color yellow $ Polygon $
	map (normalizedPosToGloss wSize destArea) [(0,0),(0,1),(1,1),(1,0),(0,0)]

renderGame :: WindowSize -> DestAreaOnScreen -> DestAreaOnScreen -> World -> Picture
renderGame wSize dbgTextArea gameArea world = Pictures $ [
	renderGameArea wSize gameArea world ,
	renderDbgText wSize dbgTextArea world
	]

renderGameArea :: WindowSize -> DestAreaOnScreen -> World -> Picture
renderGameArea wSize destArea world = Color green $ Polygon $
	map (normalizedPosToGloss wSize destArea) [(0,0),(0,1),(1,1),(1,0),(0,0)]


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
			where lineHeight = (1-yPos) / (fromIntegral $ length lines)
--Color violet $ Polygon $
	--map (normalizedPosToGloss wSize destArea) [(0,0),(0,1),(1,1),(1,0),(0,0)]


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
