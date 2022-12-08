import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Ord

main = do
  input <- readFile "input.txt"
  let elf = [ sum (map read x :: [Integer]) | x <- map lines $ splitOn "\n\n" input]
  print $ maximum elf
  let topThree = sum $ take 3 $ sortOn Down elf
  print topThree
