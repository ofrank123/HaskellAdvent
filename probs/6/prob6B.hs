import Data.List.Split
import Data.List
import Data.Ord

main :: IO ()
main = do
  contents <- getContents
  let coords = getCoordL $ init $ splitOn "\n" contents
  print coords
  let cmap = getMap coords
  --printMap cmap
  let regMap = drawReg coords cmap
  --printMap regMap
  print "Region Size:"
  print $ regionSize regMap

regionSize :: [[Int]] -> Int
regionSize cmap = sum (map sum cmap)

drawReg :: [(Int, Int)] -> [[Int]] -> [[Int]]
drawReg coords cmap = _drawReg (0, 0) coords cmap

_drawReg :: (Int, Int) -> [(Int, Int)] -> [[Int]] -> [[Int]]
_drawReg (x, y) coords cmap
  | y == length cmap = cmap
  | x == length (head cmap) = _drawReg (0, y+1) coords cmap
  | (calcTotalDist (x, y) coords) < 10000 = _drawReg (x+1, y) coords (setCoord 1 (x, y) cmap)
  | otherwise = _drawReg (x+1, y) coords cmap

calcTotalDist :: (Int, Int) -> [(Int, Int)] -> Int
calcTotalDist coord coords = sum [calcManhattanDist a coord | a <- coords]

calcManhattanDist :: (Int, Int) -> (Int, Int) -> Int
calcManhattanDist (x0, y0) (x1, y1) = (abs (x0 - x1)) + (abs (y0 - y1))


--Print funcs

printMap :: [[Int]] -> IO ()
printMap arr
  | null arr = do
      putChar '\n'
  | otherwise = do
      printLine (head arr)
      printMap (tail arr)

printLine :: [Int] -> IO ()
printLine arr
  | null arr = do
      putChar '\n'
  | (head arr == -1) = do
      putStr "  "
      putChar '.'
      printLine (tail arr)
  | (head arr >= 10) = do
      putChar ' '
      putStr $ show (head arr)
      printLine (tail arr)
  | otherwise = do
      putChar ' '
      putStr (" " ++ show (head arr))
      printLine (tail arr)


--Map funcs

getCoord :: (Int, Int) -> [[Int]] -> Int
getCoord (x, y) cmap = (cmap !! y) !! x

setCoord :: Int -> (Int, Int) -> [[Int]] -> [[Int]]
setCoord new (x, y) cmap = (fst $ splitAt y cmap) ++ [newYRow] ++ (tail (snd $ splitAt y cmap))
  where oldYRow = (cmap !! y)
        newYRow = (fst $ splitAt x oldYRow) ++ [new] ++ (tail (snd $ splitAt x oldYRow))



--SETUP FUNCS

getCoordL :: [String] -> [(Int, Int)]
getCoordL arr
  | null arr = []
  | otherwise = (read ((splitOn ", " (head arr)) !! 0) :: Int, read ((splitOn ", " (head arr)) !! 1) :: Int) : (getCoordL $ tail arr)

getMap :: [(Int, Int)] -> [[Int]]
getMap coords = [[0 | _ <- [0..maxX]] | _ <- [0..maxY]]
  where maxX = fst $ maximum coords
        maxY = snd $ maximumBy (comparing snd) coords
