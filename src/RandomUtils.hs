module RandomUtils where



import Control.Monad.Random
import System.Random

import GameData


-- returns a distribution of directions, given any "favourite" direction:
{-randomDirS :: (RandomGen gen) => (Direction,Rational) -> Rand gen Movement
randomDirS (preference,prob) = fromList $ (preference,prob) :
	zip (orthogonal preference) (repeat $ (1-prob)/2)
	-}

randomDirS :: (RandomGen g) => [Direction] -> [(Direction,Rational)]-> Rand g Direction
randomDirS dirList preferences = fromList $ randomDirS' dirList preferences
		
randomDirS' :: [Direction] -> [(Direction,Rational)]-> [(Direction,Rational)]
randomDirS' dirList preferences = 
	case preferences of
		[] -> zip dirList (repeat $ 1/(fromIntegral $ length dirList))
		(preference,prob): otherPrefs ->
			if (not $ preference `elem` dirList)
			then randomDirS' dirList otherPrefs
			else
				(preference,prob) : 
				(map (\(d,p) -> (d, p -prob/(fromIntegral $ length dirList-1))) $ randomDirS' (filter (/=preference) dirList) otherPrefs)
