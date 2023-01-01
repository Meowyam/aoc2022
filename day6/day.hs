import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Char

main = do
  input <- readFile "input.txt"
  print $ check input 0

check (l:ls) acc
  | (length $ nub (l:(take 13 ls))) == 14 = acc + 14
  | otherwise = check ls (acc + 1)
