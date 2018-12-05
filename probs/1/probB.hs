import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let ints = cycle $ stringToIntList $ init $ splitOn "\n" contents
  print $ stringToIntList $ init $ splitOn "\n" contents
  print $ findFirstDupSum ints


stringToIntList :: [String] -> [Integer]
stringToIntList str = [if head x == '+'
                        then read( tail x ) :: Integer
                        else read( x )
                      | x <- str]

findFirstDupSum :: [Integer] -> Integer
findFirstDupSum arr = _findFirstDupSum arr [0]

_findFirstDupSum :: [Integer] -> [Integer] -> Integer
_findFirstDupSum arr sums
  | (head arr + last sums) `elem` sums = (head arr + last sums)
  | otherwise = _findFirstDupSum (tail arr) (sums ++ [(head arr + last sums)])
