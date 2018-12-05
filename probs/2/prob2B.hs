import Data.List.Split

main :: IO ()
main = do
  contents <- getContents
  let codes = init $ splitOn "\n" contents
  print $ findCodeMatch codes

findCodeMatch :: [String] -> String
findCodeMatch arr
  | null arr = ""
  | (fst $ checkCharDiffs (head arr) (tail arr)) /= "" =
    getSimilarLetters $ checkCharDiffs (head arr) (tail arr)
  | otherwise = findCodeMatch (tail arr)

getSimilarLetters :: (String, String) -> String
getSimilarLetters (a, b)
  | null a || null b = []
  | (head a) == (head b) = head a : getSimilarLetters (tail a, tail b)
  | otherwise = getSimilarLetters (tail a, tail b)

checkCharDiffs :: String -> [String] -> (String, String)
checkCharDiffs str arr
  | null arr = ("", "")
  | numCharDiffs(str, (head arr)) == 1 = (str, head arr)
  | otherwise = checkCharDiffs str (tail arr)

numCharDiffs :: (String, String) -> Integer
numCharDiffs (a, b)
  | null a || null b = 0
  | head a == head b = numCharDiffs (tail a, tail b)
  | otherwise = 1 + numCharDiffs (tail a, tail b)
