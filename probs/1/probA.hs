import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  print $ addStrings $ init $ splitOn "\n" contents

addStrings :: [String] -> Integer
addStrings array
  | array == [] = 0
  | head ( head array ) == '+' = ( read (tail $ head $ array) :: Integer ) + addStrings ( tail array )
  | otherwise = (read (head array) :: Integer ) + addStrings (tail array)
