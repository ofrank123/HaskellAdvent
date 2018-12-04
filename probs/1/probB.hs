import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let ints = cycle $ stringToIntList $ init $ splitOn "\n" contents
  let sums = sumPrev ints
  print $ findDup $ sums


stringToIntList :: [String] -> [Integer]
stringToIntList str = [if head x == '+'
                        then read( tail x ) :: Integer
                        else read( x )
                      | x <- str]

sumPrev :: [Integer] -> [Integer]
sumPrev [] = []
sumPrev arr = sumPrev ( init arr ) ++ [( sum $ init arr) + last arr]

findDup :: [Integer] -> Integer
findDup arr = findDup_h arr 0

findDup_h :: [Integer] -> Int -> Integer
findDup_h arr i
  | any ((arr !! i) ==) (fst $ splitAt i arr) = arr !! i
  | otherwise = findDup_h arr (i + 1)
