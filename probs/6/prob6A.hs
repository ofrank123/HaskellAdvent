import Data.List.Split
import Data.List
import Data.Ord

main :: IO ()
main = do
  contents <- getContents
  let coords = getCoordL $ init $ splitOn "\n" contents
  print coords
  --printMap $ getMap coords
  let cmap = setCoordsOnMap coords (getMap coords)
  let finMap = calcSquares coords cmap
  --printMap $ finMap
  print "Bounded:"
  let bounded = getBounded coords finMap
  print bounded
  print "Largest Bounded Region:"
  print $ getLargest bounded finMap

getLargest :: [Int] -> [[Int]] -> Int
getLargest regions cmap = fst (maximum [a | a <- zip [countNums b cmap | b <- regions] regions])

countNums :: Int -> [[Int]] -> Int
countNums n cmap
  | null cmap = 0
  | otherwise = (length $ filter (n==) (head cmap)) + countNums n (tail cmap)

getBounded :: [(Int, Int)] -> [[Int]] -> [Int]
getBounded coords cmap = [a | a <- nums, not $ onEdge a cmap]
  where nums = [x | x <- [0..((length coords) - 1)]]

onEdge :: Int -> [[Int]] -> Bool
onEdge val cmap = length [a | a <- [(x, y) | x <- [0..((length (head cmap)) - 1)], y <- [0..((length cmap) - 1)]], (getCoord a cmap) == val && (coordOnEdge a cmap)] > 0

coordOnEdge :: (Int, Int) -> [[Int]] -> Bool
coordOnEdge (0, _) _= True
coordOnEdge (_, 0) _= True
coordOnEdge (x, y) cmap
  | x == (length (head cmap) - 1) = True
  | y == (length cmap - 1) = True
  | otherwise = False

calcSquares :: [(Int, Int)] -> [[Int]] -> [[Int]]
calcSquares coords cmap = _calcSquares (0,0) cmap coords

_calcSquares :: (Int, Int) -> [[Int]] -> [(Int, Int)] -> [[Int]]
_calcSquares (x, y) cmap coords
  | y == length cmap = cmap
  | x == length (head cmap) = _calcSquares (0, y+1) cmap coords
  | otherwise = let shortest = findShortest (x, y) coords in
      if length shortest == 1
      then _calcSquares (x+1, y) (setCoord (getCoord (shortest !! 0) cmap) (x, y) cmap) coords
      else _calcSquares (x+1, y) (setCoord (-1) (x, y) cmap) coords

findShortest :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
findShortest coord coords = [fst y | y <- [x | x <- sortedCoords, (snd (head sortedCoords) == snd x)]]
  where dists = zip coords (calcDists coord coords)
        sortedCoords = sortBy (comparing snd) dists

calcDists :: (Int, Int) -> [(Int, Int)] -> [Int]
calcDists (x, y) coords
  | null coords = []
  | otherwise = calcManhattanDist (x, y) (head coords) : calcDists (x, y) (tail coords)

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

--Setup Funcs

getCoordL :: [String] -> [(Int, Int)]
getCoordL arr
  | null arr = []
  | otherwise = (read ((splitOn ", " (head arr)) !! 0) :: Int, read ((splitOn ", " (head arr)) !! 1) :: Int) : (getCoordL $ tail arr)

getMap :: [(Int, Int)] -> [[Int]]
getMap coords = [[-1 | _ <- [0..maxX]] | _ <- [0..maxY]]
  where maxX = fst $ maximum coords
        maxY = snd $ maximumBy (comparing snd) coords

setCoordsOnMap :: [(Int, Int)] -> [[Int]] -> [[Int]]
setCoordsOnMap coords cmap = _setCoordsOnMap coords 0 cmap

_setCoordsOnMap :: [(Int, Int)] -> Int -> [[Int]] -> [[Int]]
_setCoordsOnMap coords ccoord cmap
  | null coords = cmap
  | otherwise = _setCoordsOnMap (tail coords) (ccoord + 1) (setCoord ccoord (head coords) cmap)

