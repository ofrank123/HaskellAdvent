import Data.List.Split
import Data.List

main :: IO ()
main = do
  contents <- getContents
  let reqA = stringsToReqs $ init $ splitOn "\n" contents
  print reqA
  let steps = getMetLetters $ reqsToSteps reqA
  print $ runSteps steps

--Step type decleration
data Step = Step { letter :: Char,
                   reqs :: [Char]
                 } deriving (Show, Eq, Ord)

allReqsMet :: [Step] -> [Char] -> [Step]
allReqsMet arr done = sortSteps $ [a | a <- arr, stepReqsMet a done]

stepReqsMet :: Step -> [Char] -> Bool
stepReqsMet step done = length [a | a <- (reqs step), a `elem` done] == length (reqs step)

sortSteps :: [Step] -> [Step]
sortSteps arr = sortBy compareSteps arr

compareSteps :: Step -> Step -> Ordering
compareSteps s1 s2
  | letter s1 > letter s2 = GT
  | letter s2 < letter s2 = LT
  | otherwise = EQ

runSteps :: [Step] -> [Char]
runSteps arr = _runSteps (allReqsMet arr []) arr []

_runSteps :: [Step] -> [Step] -> [Char] -> [Char]
_runSteps reqsMet steps done
  | null steps = []
  | otherwise = (letter (head reqsMet)) : _runSteps (allReqsMet (filter (/=(head reqsMet)) steps) ((letter (head reqsMet)) : done)) (filter (/=(head reqsMet)) steps) ((letter (head reqsMet)) : done) 

getMetLetters :: [Step] -> [Step]
getMetLetters arr = arr ++ [Step a [] | a <- initDoneLetters arr]

--SETUP FUNCS

stringsToReqs :: [String] -> [(Char, Char)]
stringsToReqs arr
  | null arr = []
  | otherwise = (((splitOn " " (head arr)) !! 1) !! 0, ((splitOn " " (head arr)) !! 7) !! 0 ): stringsToReqs (tail arr)

reqsToSteps :: [(Char, Char)] -> [Step]
reqsToSteps rArr = _reqsToSteps rArr []

_reqsToSteps :: [(Char, Char)] -> [Step] -> [Step]
_reqsToSteps rArr sArr
  | null rArr = sArr
  | null [x | x <- sArr, (letter x) == snd (head rArr)] = _reqsToSteps (tail rArr) (Step (snd (head rArr)) [fst (head rArr)] : sArr)
  | otherwise = _reqsToSteps (tail rArr) (updateStepArr (addStepReq (getReq (snd $ head rArr) sArr) (fst $ head rArr) ) sArr)

updateStepArr :: Step -> [Step] -> [Step]
updateStepArr new arr = [if (letter a) == letter new then new else a | a <- arr]

addStepReq :: Step -> Char -> Step
addStepReq step req = Step (letter step) (req : (reqs step))

getReq :: Char -> [Step] -> Step
getReq c arr = [a | a <- arr, (letter a) == c] !! 0

getLetters :: [Step] -> [Char]
getLetters arr = [letter a | a <- arr]

getReqs :: [Step] -> [Char]
getReqs arr
  | null arr = []
  | otherwise = (reqs $ head arr) ++ getReqs (tail arr)

initDoneLetters :: [Step] -> [Char]
initDoneLetters arr = [a | a <- ['A'..'Z'], (not (a `elem` letters)) && (a `elem` (getReqs arr))]
  where letters = getLetters arr
