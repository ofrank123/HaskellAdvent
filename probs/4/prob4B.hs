import Data.List.Split
import Data.List
import Data.Ord

data Guard = Guard {gid :: Integer,
                    minutes :: [Integer]
                   } deriving (Show, Eq) 

main :: IO ()
main = do
  contents <- getContents
  let logs = sortBy compTime (init $ splitOn "\n" contents)
  --print logs
  print (getGuardMins logs (nub $ getGuardLists logs) 0)
  let guardsMaxMins = guardMaxMins $ getGuardMins logs (nub $ getGuardLists logs) 0
  print guardsMaxMins
  print $ maximumBy (comparing snd) guardsMaxMins

guardMaxMins :: [Guard] -> [(Integer, (Integer, Integer))]
guardMaxMins ga
  | null ga = []
  | otherwise = (cgid, maxminute) : guardMaxMins (tail ga)
  where currentG = head ga
        cgid = gid currentG
        maxminute = maximum [a | a <- zip (minutes currentG) [0..]]


getGuardMins :: [String] -> [Guard] -> Integer -> [Guard]
getGuardMins logs guardL guardid
  | null logs = guardL
  | keyw == "Guard" = getGuardMins (tail logs) guardL (read (tail $ (splitOn " " (head logs)) !! 3) :: Integer)
  | keyw == "falls" = getGuardMins (tail $ tail logs) (updateGuardL guardL (incrMins (getGuardByGid guardL guardid) smin emin)) guardid
  | otherwise = [] --Shouldn't get here
  where keyw = (splitOn " " (head logs)) !! 2
        sdate = tail $ (splitOn "] " (head logs)) !! 0
        edate = tail $ (splitOn "] " (head $ tail logs)) !! 0
        smin = read ((splitOn ":" ((splitOn " " sdate) !! 1)) !! 1) :: Integer
        emin = read ((splitOn ":" ((splitOn " " edate) !! 1)) !! 1) :: Integer


updateGuardL :: [Guard] -> Guard -> [Guard]
updateGuardL guardL g
  | null guardL = []
  | (gid $ head guardL) == gid g = g : tail guardL
  | otherwise = (head guardL) : updateGuardL (tail guardL) g

getGuardByGid :: [Guard] -> Integer -> Guard
getGuardByGid ga guardid
  | null ga = Guard 0 [0]
  | gid (head ga) == guardid = head ga
  | otherwise = getGuardByGid (tail ga) guardid

guardInit :: Guard -> Guard
guardInit g = Guard (gid g) ([0 | _ <- [0..59]])

incrMins :: Guard -> Integer -> Integer -> Guard
incrMins g start end = Guard (gid g) [ if i >= start && i < end then x + 1 else x | (i, x) <- zip [0..] minuteA]
  where minuteA = minutes g

getGuardLists :: [String] -> [Guard]
getGuardLists arr
  | null arr = []
  | first == "Guard" = guardInit (Guard (read (tail ((splitOn " " (head arr)) !! 3)) :: Integer) []) : getGuardLists(tail arr)
  | otherwise = getGuardLists(tail arr)
  where first = (splitOn " " (head arr)) !! 2

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



