import Data.List.Split
import Data.List

main :: IO ()
main = do
  contents <- getContents
  let squares = getSquares $ init $ splitOn "\n" contents
  print $ findUnclaim $ squares

-- (x0, y0, x, y)
getSquares :: [String] -> [(Integer, Integer, Integer, Integer, Integer)]
getSquares arr
  | null arr = []
  | otherwise = (n, x0, y0, x, y) : getSquares(tail arr)
  where line = (splitOn " @ " (head arr)) !! 1
        n = read $ tail $ ((splitOn " @ " (head arr)) !! 0) :: Integer
        x0 = read $ (splitOn "," ((splitOn ": " line) !! 0)) !! 0 :: Integer
        y0 = read $ (splitOn "," ((splitOn ": " line) !! 0)) !! 1 :: Integer
        x = read $ (splitOn "x" ((splitOn ": " line) !! 1)) !! 0 :: Integer
        y = read $ (splitOn "x" ((splitOn ": " line) !! 1)) !! 1 :: Integer

findUnclaim :: [(Integer, Integer, Integer, Integer, Integer)] -> Integer
findUnclaim carr = _findUnclaim carr 0

_findUnclaim :: [(Integer, Integer, Integer, Integer, Integer)] -> Int -> Integer
_findUnclaim carr i
  | i == length carr = -1
  | null $ getIntersects claim (filter (claim /=) carr) = firstEl claim
  | otherwise = _findUnclaim carr (i+1)
  where claim = carr !! i

firstEl :: (Integer, Integer, Integer, Integer, Integer) -> Integer
firstEl (a,_,_,_,_) = a
    
calcClaims :: (Integer, Integer, Integer, Integer, Integer) -> [(Integer, Integer)]
calcClaims (_, x0, y0, x, y) =
  [(a, b) | a <- [x0 .. (x + x0 - 1)], b <- [y0 .. (y + y0 - 1)]]

getIntersects :: (Integer, Integer, Integer, Integer, Integer) -> [(Integer, Integer, Integer, Integer, Integer)] -> [(Integer, Integer)]
getIntersects claim carr
  | null carr = []
  | otherwise = (calcClaims claim) `intersect` (calcClaims $ head carr) ++ getIntersects claim (tail carr)
