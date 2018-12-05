import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let codes = init $ splitOn "\n" contents
  print $ checksum codes

checksum :: [String] -> Integer
checksum arr = letterDupCount arr 2 * letterDupCount arr 3

letterDupCount :: [String] -> Int -> Integer
letterDupCount arr n
  | null arr = 0
  | checkLetterDups (head arr) n = 1 + letterDupCount (tail arr) n
  | otherwise = letterDupCount (tail arr) n

checkLetterDups :: String -> Int-> Bool
checkLetterDups str n
  | null str = False
  | length (filter (head str==) str) == n = True
  | otherwise = checkLetterDups (filter (head str /=) (tail str)) n
