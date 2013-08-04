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
renderMenu _ = Pictures [ (Translate (-140) (230) $ Scale 0.5 0.5 $ Color white $ Text "Pa man"),
    (Translate (-36)(248) $ Scale 0.25 (0.25) $ pacmanbase),
    (Translate (-36)(248) $ Scale 0.25 (0.25) $ moveright),
    (Translate (-75) (120) $ Scale 0.4 (0.4) $ Color white $ Text "Menu"),
    (Translate (-90) (30) $ Scale 0.25 (0.25) $ Color white $ Text "[s]  Start"),
    (Translate (-90) (-30) $ Scale 0.25 (0.25) $ Color white $ Text "[Esc] Exit")]

renderCharacters :: Characters -> AreaOnScreen -> Picture
renderCharacters chars area = undefined

renderItems :: Items -> AreaOnScreen -> Picture
renderItems = undefined

renderLabyrinth :: Labyrinth -> AreaOnScreen ->  Picture
renderLabyrinth labyrinth (posOnScreen, sizeOnScreen) = undefined

screenPosFromPos :: Area -> Pos -> AreaOnScreen -> PosOnScreen
screenPosFromPos (fieldPos,fieldSize) pos (fPosOnS,fSizeOnS) = fPosOnS <+> (vecF pos </> vecF fieldSize <*> fSizeOnS)

vecF = fOnVec fromIntegral -- floats parameters
vecI = fOnVec floor -- integrates parameters using floor

-- ingame top-left-zero coordinates to gloss centered-zero coordinates
i2C :: AreaOnScreen -> PosOnScreen -> PosOnScreen
i2C (_, sizeOnScreen) (x, y) = (transfX, transfY)
    where   transfX = x -(fst sizeOnScreen)/2 -- simple translation from [0, width] -> [-width/2, +width/2]
            transfY = -(y -(snd sizeOnScreen)/2) -- translation plus flip from [0, height] -> [+height/2, -height/2]

-- pacman
pacup = map (scale 0.1 0.1) [pacmanbase, moveup]
pacdown = map (scale 0.1 0.1) [pacmanbase, movedown]
pacright = map (scale 0.1 0.1) [pacmanbase, moveright]
pacleft = map (scale 0.1 0.1) [pacmanbase, moveleft]
pacdemo = map (translate 100 100) pacup ++ map (translate (-100) 100) pacdown ++ map (translate (-100) (-100)) pacleft ++ map (translate (100) (-100)) pacright
pacmanbase = Color yellow (ThickCircle 40 80)
moveright = Color black (Polygon [(0,0), (80,(40)), (80,(-40))])
moveleft = rotate 180 moveright
moveup = rotate (-90) moveright
movedown = rotate 90 moveright
