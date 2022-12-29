import System.IO
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Map as Map 

main = do
  input <- readFile "input.txt"
  let pairs = map words $ lines input
  let pairVals m = [(fromJust $ Map.lookup (head x) m, fromJust $ Map.lookup (concat $ tail x) m) | x <- pairs]
  print $ sum [(match x win) + (snd x) | x <- (pairVals mapVals)]
  print $ sum [match2 x win + (snd x) | x <- (pairVals sndVals)]

--a,x is rock
--b,y is paper
--c,z is scissors
mapVals :: Map.Map String Int
mapVals = Map.fromList [("A",1),("X",1),("B",2),("Y",2),("C",3),("Z",3)]

sndVals :: Map.Map String Int
sndVals = Map.fromList [("A",1),("X",0),("B",2),("Y",3),("C",3),("Z",6)]

secnd :: (Int,Int,Int) -> Int
secnd (_,x,_) = x

third :: (Int,Int,Int) -> Int
third (_,_,x) = x

match :: (Int,Int) -> [(Int,Int,Int)] -> Int
match (a,b) ((x,y,z):ls)
  | (a == x) && (b == y) = z
  | otherwise = match (a,b) ls

match2 :: (Int,Int) -> [(Int,Int,Int)] -> Int
match2 (a,b) ((x,y,z):ls)
  | (a == x) && (b == z) = y
  | otherwise = match2 (a,b) ls

win :: [(Int,Int,Int)]
win = [(1,2,6)
      ,(2,3,6)
      ,(3,1,6)
      ,(1,1,3)
      ,(2,2,3)
      ,(3,3,3)
      ,(1,3,0)
      ,(2,1,0)
      ,(3,2,0)]
