import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Char
import Data.Ord

main = do
  input <- readFile "input.txt"
  let ruckparts = [chunksOf (length x `div` 2) x | x <- lines input]
  let groups = chunksOf 3 (lines input)
  print groups
  let clusters = [concat $ map sort x | x <- map (map nub) groups]
  print $ count $ concat $ [ nub $ head x `intersect` last x | x <- ruckparts ]
  print $ count $ map (head . maximumBy (comparing length) . group . sort) $ clusters

count :: [Char] -> Int
count [] = 0 
count (x:xs)
  | isUpper x = (ord (toLower x) - tare + 26) + (count xs)
  | otherwise = (ord x - tare) + (count xs)
  where tare = ord 'a' - 1
