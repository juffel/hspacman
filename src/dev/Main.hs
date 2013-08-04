{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Graphics.Gloss

main = display (InWindow "PacMan Study" (400, 400) (10, 10)) black (Pictures pacdemo)

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

--ghost x = Color x ()
