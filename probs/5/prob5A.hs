import Data.Char

main :: IO ()
main = do
  contents <- getContents
  print $ length $ reactPoly $ init contents

reactPoly :: [Char] -> [Char]
reactPoly poly
  | polyPass poly == poly = poly
  | otherwise = reactPoly $ polyPass poly

polyPass :: [Char] -> [Char]
polyPass poly = _polyPass poly '-'

_polyPass :: [Char] -> Char -> [Char]
_polyPass poly lmono
  | null poly = []
  | null (tail poly) = mono : _polyPass (tail poly) mono
  | (monoReact mono (head (tail poly))) = _polyPass (tail (tail poly)) mono
  | otherwise = mono : _polyPass (tail poly) mono
  where mono = if null poly then '-' else head poly

monoReact :: Char -> Char -> Bool
monoReact mono1 mono2 = (toLower mono1 == toLower mono2) && (xor lower1 lower2)
  where lower1 = toLower mono1 == mono1
        lower2 = toLower mono2 == mono2

xor :: Bool -> Bool -> Bool
xor True x = not x
xor False x = x
