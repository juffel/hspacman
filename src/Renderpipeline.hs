module Renderpipeline where

import GameData
import LevelGenerator
import Vector2D

import Graphics.Gloss hiding(display)
import qualified Graphics.Gloss as G

type PosOnScreen = Vec Float
type SizeOnScreen = Vec Float
type AreaOnScreen = (PosOnScreen,SizeOnScreen)

renderWorld :: AreaOnScreen -> World -> Picture
renderWorld areaOnScreen world = case (uiState $ settings world) of -- check wether the game is currently in status Playing or Menu
    Playing ->  (uncurry Translate) (i2C areaOnScreen (fromIntegral 0, fromIntegral 0)) $ -- Translation
                Scale 1 (-1) $ -- flip y-axis
                renderPlaying areaOnScreen world

    Menu -> renderMenu world

renderPlaying :: AreaOnScreen -> World -> Picture
renderPlaying areaOnScreen world = Pictures $
	[renderCharacters characters' areaOnScreen, renderItems items' areaOnScreen, renderLabyrinth lab areaOnScreen]
	where	lab = labyrinth $ game world
		items' = items $ game world 
		characters' = characters $ game world

renderMenu :: World -> Picture
renderMenu _ = Pictures [ Text "Pacman - The Menu" ]

renderCharacters :: Characters -> AreaOnScreen -> Picture
renderCharacters chars area = undefined

renderItems :: Items -> AreaOnScreen -> Picture
renderItems = undefined

renderLabyrinth :: Labyrinth -> AreaOnScreen ->  Picture
renderLabyrinth lab (posOnScreen, sizeOnScreen) = mapWithIndex drawCell lab
    where
        drawCell :: MatrIndex -> Territory -> Picture
        drawCell coords ter = case ter of
            Free -> Color white $ drawRectangle (screenPosFromPos coords) (screenPosFromPos size)
            Wall -> Color black $ drawRectangle (screenPosFromPos coords) (screenPosFromPos size)

drawRectangle :: Pos -> Size -> Picture 
drawRectangle pos size = Polygon [ pos, pos <+> (VecX size), pos <+> size, pos <+> (vecY size), pos ]

-- |converts virtual board coordinates @pos@ on virtual board @lab@ to real screen coordinates on the @areaOnScreen@
-- which is usesd by the sub renderfunctions like renderItems, renderCharacters...
-- so that these sub functions do not need to transfer virtual coordinates to real screen coordinates theirselves
screenPosFromPos :: Labyrinth -> Pos -> AreaOnScreen -> PosOnScreen
screenPosFromPos lab pos areaOnScreen@(fPosOnS,fSizeOnS) = fPosOnS <+> (vecF pos </> vecF fieldSize <*> fSizeOnS)

vecF = fOnVec fromIntegral -- floats parameters
vecI = fOnVec floor -- integrates parameters using floor

-- |ingame top-left-zero coordinates to gloss centered-zero coordinates
-- only used in renderWorld, when finally rendering the world on screen
i2C :: AreaOnScreen -> PosOnScreen -> PosOnScreen
i2C (_, sizeOnScreen) (x, y) = (transfX, transfY)
    where   transfX = x -(fst sizeOnScreen)/2 -- simple translation from [0, width] -> [-width/2, +width/2]
            transfY = -(y -(snd sizeOnScreen)/2) -- translation plus flip from [0, height] -> [+height/2, -height/2]
