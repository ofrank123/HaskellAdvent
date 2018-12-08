import Data.List.Split
import Data.List

main :: IO ()
main = do
  contents <- getContents
  let squares = getSquares $ init $ splitOn "\n" contents
  print $ allClaimIntersects squares

-- (x0, y0, x, y)
getSquares :: [String] -> [(Integer, Integer, Integer, Integer)]
getSquares arr
  | null arr = []
  | otherwise = (x0, y0, x, y) : getSquares(tail arr)
  where line = (splitOn " @ " (head arr)) !! 1
        x0 = read $ (splitOn "," ((splitOn ": " line) !! 0)) !! 0 :: Integer
        y0 = read $ (splitOn "," ((splitOn ": " line) !! 0)) !! 1 :: Integer
        x = read $ (splitOn "x" ((splitOn ": " line) !! 1)) !! 0 :: Integer
        y = read $ (splitOn "x" ((splitOn ": " line) !! 1)) !! 1 :: Integer

calcClaims :: (Integer, Integer, Integer, Integer) -> [(Integer, Integer)]
calcClaims (x0, y0, x, y) =
  [(a, b) | a <- [x0 .. (x + x0 - 1)], b <- [y0 .. (y + y0 - 1)]]

getIntersects :: (Integer, Integer, Integer, Integer) -> [(Integer, Integer, Integer, Integer)] -> [(Integer, Integer)]
getIntersects claim carr
  | null carr = []
  | otherwise = (calcClaims claim) `intersect` (calcClaims $ head carr) ++ getIntersects claim (tail carr)

allClaimIntersects :: [(Integer, Integer, Integer, Integer)] -> [(Integer, Integer)]
allClaimIntersects arr
  | null arr = []
  | otherwise = (getIntersects (head arr) (tail arr)) ++ (allClaimIntersects $ tail arr)

