import System.IO
import Control.Monad
import Data.List.Split

main = do
  input <- readFile "input.txt"
  let elf = [ sum (map read x :: [Integer]) | x <- map lines $ splitOn "\n\n" input]
  print $ maximum elf
