import Data.List.Split
import Data.List

main :: IO ()
main = do
  contents <- getContents
  let logs = sortBy compTime (init $ splitOn "\n" contents)
  print $ getGuardLists logs

getGuardLists :: [String] -> [(Integer, [Integer])]
getGuardLists arr
  | null arr = []
  | first == "Guard" = (read (tail ((splitOn " " (head arr)) !! 3)) :: Integer, []) : getGuardLists(tail arr)
  | otherwise = getGuardLists(tail arr)
  where first = (splitOn " " (head arr)) !! 2

getGuardMinutes :: [String] -> (Integer, [Integer]) -> (Integer, [Integer])
getGuardMinutes arr guard = _getGuardMinutes arr False guard

_getGuardMinutes :: [String] -> Bool -> (Integer, [Integer]) -> (Integer, [Integer])
_getGuardMinutes arr inShift (id, minutes)
  | null arr = (id, minutes)
  | ((splitOn " " current) !! 2 == "Guard") = if (read (head ((splitOn " " current) !! 3)) :: Integer) == id
                                              then _getGuardMinutes (tail arr) True (id, minutes)
                                              else _getGuardMinutes (tail arr) False (id, minutes)
  | 

  where current = head arr

intToOrdering :: Integer -> Ordering
intToOrdering i
  | i < 0 = LT
  | i > 0 = GT
  | otherwise = EQ

compTime :: [Char] -> [Char] -> Ordering
compTime str1 str2
  | year1 /= year2 = intToOrdering $ year1 - year2
  | month1 /= month2 = intToOrdering $ month1 - month2
  | day1 /= day2 = intToOrdering $ day1 - day2
  | hour1 /= hour2 = intToOrdering $ hour1 - hour2
  | otherwise = intToOrdering $ min1 - min2
  where date1 = tail $ (splitOn "] " str1) !! 0
        date2 = tail $ (splitOn "] " str2) !! 0
        year1 = read ((splitOn "-" date1) !! 0) :: Integer
        year2 = read ((splitOn "-" date2) !! 0) :: Integer
        month1 = read ((splitOn "-" date1) !! 1) :: Integer
        month2 = read ((splitOn "-" date2) !! 1) :: Integer
        day1 = read ((splitOn " " ((splitOn "-" date1) !! 2)) !! 0) :: Integer
        day2 = read ((splitOn " " ((splitOn "-" date2) !! 2)) !! 0) :: Integer
        hour1 = read ((splitOn ":" ((splitOn " " date1) !! 1)) !! 0) :: Integer
        hour2 = read ((splitOn ":" ((splitOn " " date2) !! 1)) !! 0) :: Integer
        min1 = read ((splitOn ":" ((splitOn " " date1) !! 1)) !! 1) :: Integer
        min2 = read ((splitOn ":" ((splitOn " " date2) !! 1)) !! 1) :: Integer



