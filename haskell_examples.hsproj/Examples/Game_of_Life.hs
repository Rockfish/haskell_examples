module Game_of_Life where
  
import Data.Array
import Data.List
import Control.Concurrent


grid = [[1, 0, 0, 0, 1],
        [0, 1, 0, 1, 0],
        [0, 0, 1, 0, 0],
        [0, 1, 0, 1, 0],      
        [1, 0, 0, 0, 1]]

pattern = [
    (-1, -1), (0, -1), (1, -1),
    (-1, 0),           (1, 0),
    (-1, 1), (0, 1), (1, 1)]

mylookup g x y = (g !! y) !! x

gLookup g xmax ymax (x, y)
  | x > -1 && y > -1 && x <= xmax && y <= ymax = (g !! y) !! x
  | otherwise = 0

boardLookup g = gLookup g 4 4

add (x, y) (a, b) = (x + a, y + b)
  
cellStatus g y x = 
  let state = boardLookup g (x, y)
      neighbors = map addPoints pattern
      neighborCount = sum (map (boardLookup g) neighbors)
  in (state, neighborCount)
  where addPoints = add (x, y)
 

rowStatus g xs y = map (cellStatus g y) xs

isLive (s, c)
  | s == 0 && c == 3 = 1
  | s == 1 && c == 2 = 1
  | s == 1 && c == 3 = 1
  | otherwise = 0
 

newRow g xs y = map isLive (rowStatus g xs y)

newGrid g = map (newRow g [0..4]) [0..4]

putRow row =
  sequence_ [putStr (show x ++ " ") | x <- row]

showCell x = if x == 1 then "*" else " "

rowStr row = intercalate " " $ map showCell row

putGrid grid = 
  sequence_ [ putStrLn $ rowStr row | row <- grid]

runGame grid = do
  putGrid grid
  threadDelay 200000
  runGame $ newGrid grid
 

getRanges grid =
  let xr = [0 .. length (head grid)]
      yr = [0 .. length grid]
  in (xr, yr)


newGrid2 grid = 
  let ranges = getRanges grid
      xr = fst ranges
      yr = snd ranges
  in map (newRow grid xr) yr


  
