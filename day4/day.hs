import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Set as S

main = do
  input <- readFile "input.txt"
  let pairs = [map ranges $ map (map (read::String->Int)) $ map (splitOn "-") $ splitOn "," x | x <- lines input]
  print $ sum $ map fromEnum [checkSet $ map S.fromList $ x | x <- pairs]
  print $ length $ filter (not . null) $ map checkList $ map sort pairs

ranges :: [Int] -> [Int]
ranges [x,y] = [x..y]

checkSet [x,y]
  | (x `S.isSubsetOf` y) = True
  | (y `S.isSubsetOf` x) = True
  | otherwise = False

checkList [x,y] = x `intersect` y 
