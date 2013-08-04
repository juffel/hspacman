module Renderpipeline where

import GameData
import LevelGenerator
import Vector2D
import Math.Matrix
import qualified Data.Foldable as F -- enables folds over matrices

import Graphics.Gloss hiding(display)
import qualified Graphics.Gloss as G

type PosOnScreen = Vec Float
type SizeOnScreen = Vec Float
type AreaOnScreen = (PosOnScreen,SizeOnScreen)

renderWorld :: AreaOnScreen -> World -> Picture
renderWorld areaOnScreen world = case (uiState world) of -- check wether the game is currently in status Playing or Menu
    Playing ->  (uncurry Translate) (i2C areaOnScreen (fromIntegral 0, fromIntegral 0)) $ -- Translation
                Scale 1 (-1) $ -- flip y-axis
                renderPlaying areaOnScreen world

    Menu -> renderMenu world

renderPlaying :: AreaOnScreen -> World -> Picture
renderPlaying areaOnScreen world = Pictures [
    renderLabyrinth lab areaOnScreen,
    renderDots dotz areaOnScreen,
    renderFruits fru areaOnScreen,
    renderPacman lab pac areaOnScreen,
    renderGhosts lab gho areaOnScreen ]
	where
        lab = labyrinth world
        fru = fruits world 
        dotz = dots world
        gho = ghosts world
        pac = pacman world

renderMenu :: World -> Picture
renderMenu _ = Pictures [ (Translate (-140) (230) $ Scale 0.5 0.5 $ Color white $ Text "Pa man"),
    (Translate (-36)(248) $ Scale 0.25 (0.25) $ pacmanbase),
    (Translate (-36)(248) $ Scale 0.25 (0.25) $ moveright),
    (Translate (-75) (120) $ Scale 0.4 (0.4) $ Color white $ Text "Menu"),
    (Translate (-90) (30) $ Scale 0.25 (0.25) $ Color white $ Text "[s]  Start"),
    (Translate (-90) (-30) $ Scale 0.25 (0.25) $ Color white $ Text "[Esc] Exit")]

renderPacman :: Labyrinth -> Object -> AreaOnScreen -> Picture
renderPacman lab pacman areaOS = Pictures [ drawPacman pacman areaOS ]
    where
        drawPacman pacman areaOS = (uncurry Translate) (screenPosFromPosF areaOS lab (pos pacman) ) $ Color yellow $
		(uncurry Scale) (scalePacMan,scalePacMan) $
		ThickCircle (1/4) (1/2)
	scalePacMan = scaleDown * (min cellWidth cellHeight)
	scaleDown = 0.7
	(cellWidth,cellHeight) = posBottomRight <-> origin
	[origin, posBottomRight] = map (screenPosFromPosF areaOS lab) [(0,0),(1,1)]
	--ThickCircle (radius/2) radius
        --radius = vecX $ screenPosFromPosF areaOS lab (0.5, 0)

renderGhosts :: Labyrinth -> [Object] -> AreaOnScreen -> Picture
renderGhosts lab ghosts areaOS =  Pictures [drawMonsters (ghosts) areaOS]
    where   drawMonsters ghosts areaOS = Blank -- TODO


renderFruits :: [Object] -> AreaOnScreen -> Picture
renderFruits items areaOS = Blank -- TODO

renderDots :: [Object] -> AreaOnScreen -> Picture
renderDots items areaOS = Blank -- TODO

-- |renders the labMatrix (consisting of fields with values (Wall | Free) ) onto the screen.
-- Thanks to the Foldable and Monoidic Matrix data type this is done quite intuitively. Like all sub render functions this function uses the @screenPosFromPos@ translation function in order to not care about the actual representation on screen
renderLabyrinth :: Labyrinth -> AreaOnScreen ->  Picture
renderLabyrinth lab areaOnScreen = Pictures $ F.foldr (:)[] $ mapWithIndex drawCell lab
    where
        -- in order to display the matrix correctly the lines/columns have to be flipped when drawing
        drawCell :: MatrIndex -> Territory -> Picture
        drawCell coords ter = case ter of
            Free -> Color chartreuse $ drawRectangle p s
            Wall -> Color aquamarine $ drawRectangle p s
            where   p = screenPosFromPos areaOnScreen lab (transpose coords) -- flip!
                    s = screenPosFromPos areaOnScreen lab (1,1) 

drawRectangle :: PosOnScreen -> SizeF -> Picture 
drawRectangle posOS size = Polygon [  posOS,
                                    posOS <+> (vecX size,0),
                                    posOS <+> size,
                                    posOS <+> (0, vecY size),
                                    posOS ]

-- |converts virtual board coordinates @pos@ on virtual board @lab@ to real screen coordinates on the @areaOnScreen@
-- which is usesd by the sub renderfunctions like renderItems, renderCharacters...
-- so that these sub functions do not need to transfer virtual coordinates to real screen coordinates theirselves
screenPosFromPos :: AreaOnScreen -> Labyrinth -> Pos -> PosOnScreen
{- screenPosFromPos lab pos areaOnScreen@(fPosOnS,fSizeOnS) = fPosOnS <+> (vecF pos </> vecF size <*> fSizeOnS)
    where   size = (mGetWidth lab, mGetHeight lab) -}
screenPosFromPos areaOS lab pos = screenPosFromPosF areaOS lab (vecF pos) 

-- |same as @screenPosFromPos@ but with input floating tuple
screenPosFromPosF :: AreaOnScreen -> Labyrinth -> PosF -> PosOnScreen
screenPosFromPosF (fPosOS, fSizeOS) lab posF = fPosOS <+> (posF </> size <*> fSizeOS)
    where   size = vecF (mGetWidth lab, mGetHeight lab)

vecF = fOnVec fromIntegral -- floats parameters
vecI = fOnVec floor -- integrates parameters using floor

-- |ingame top-left-zero coordinates to gloss centered-zero coordinates
-- only used in renderWorld, when finally rendering the world on screen
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
