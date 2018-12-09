import Data.Char

main :: IO ()
main = do
  contents <- getContents
  print $ testMonoRemove $ init contents

testMonoRemove :: [Char] -> Int
testMonoRemove arr = minimum [(length $ reactPoly $ a) | a <- testPolys]
  where testPolys = [[x | x <- arr, toLower x /= letter] | letter <- ['a'..'z']]

reactPoly :: [Char] -> [Char]
reactPoly poly
  | polyPass poly == poly = poly
  | otherwise = reactPoly $ polyPass poly

polyPass :: [Char] -> [Char]
polyPass poly 
  | null poly = []
  | null (tail poly) = mono : polyPass (tail poly)
  | (monoReact mono (head (tail poly))) = polyPass (tail (tail poly))
  | otherwise = mono : polyPass (tail poly)
  where mono = if null poly then '-' else head poly

monoReact :: Char -> Char -> Bool
monoReact mono1 mono2 = (toLower mono1 == toLower mono2) && (xor lower1 lower2)
  where lower1 = toLower mono1 == mono1
        lower2 = toLower mono2 == mono2

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x
