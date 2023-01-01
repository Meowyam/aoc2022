import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Char

main = do
  input <- readFile "input.txt"
  let boxesandinstructs = splitOn [""] $ lines input
  let boxes = head boxesandinstructs
  -- move 1 from 2 to 1
  let instructs = map words $ last boxesandinstructs
  let ln = length (last boxes)
  let numberCols = digitToInt $ last $ filter (not . isSpace) $ last boxes
  let splitBoxes = [map (filter isAlphaNum) x | x <- (map (cols (ln `div` (numberCols - 1))) boxes)]
  let stacks = map (filter (not . null)) $ reverse $ makeStack (numberCols) splitBoxes
  print stacks
  print $ concat $ map (filter isAlpha) $ concat $ [ map head x | x <- move stacks (instructs)]

cols x = chunksOf x

move s (i:is) = case (i:is) of 
                    (i:[]) -> [(swap from newFrom (swap to newTo s))]
                    (i:is) -> move (swap from newFrom (swap to newTo s)) is
           where
             no = read (i !! 1) :: Int
             from = (read (i !! 3) :: Int) - 1
             to = (read (i !! 5) :: Int) - 1
             newFrom
               | length f > no = drop no f 
               | length f == no = drop (no - 1) f
               | otherwise = drop (length f - 1) f 
               where f = (s !! from)
             newTo
               | length f > no = ((take no f)) ++ (s !! to)
               | length f == no = ((take (no - 1) f)) ++ (s !! to)
               | otherwise =  ((take (length f - 1) f)) ++ (s !! to)
               where f = (s !! from)

makeStack :: Int -> [[String]] -> [[String]] 
makeStack 0 ls = []
makeStack n ls = getCols (n-1) ls : makeStack (n-1) ls
  where
    getCols n (x:[]) = [x !! n]
    getCols n (x:xs) = x !! n : getCols n xs

swap :: Int -> [String] -> [[String]] -> [[String]] 
swap i x y = (take (i) y) ++ [x] ++ (drop (i+1) y)
