import Data.List
import Data.List.Utils
import Data.Char
import qualified Data.Map as M

type Alphabet = M.Map Char String

main = interact (unlines . map (spell natoAlphabet) . lines)

spell :: Alphabet -> String -> String
spell a s = join " " (worker a s)
  where worker _ [] = []
        worker a (x:xs) = case M.lookup x a of
          Just y -> y : worker a xs
          Nothing -> error $ "No way to represent " ++ show x

natoAlphabet :: Alphabet
natoAlphabet = M.fromList (numerals ++ natoBase)

numerals = [
  ('0', "zero"),
  ('1', "one"),
  ('2', "two"),
  ('3', "three"),
  ('4', "four"),
  ('5', "five"),
  ('6', "six"),
  ('7', "seven"),
  ('8', "eight"),
  ('9', "nine")
  ]

natoBase = map (\x -> (head x, x)) (natoBaseList ++ map (map toUpper) natoBaseList)
natoBaseList = [
  "alpha",
  "bravo",
  "charlie",
  "delta",
  "echo",
  "foxtrot",
  "golf",
  "hotel",
  "india",
  "juliet",
  "kilo",
  "lima",
  "mike",
  "november",
  "oscar",
  "papa",
  "quebec",
  "romeo",
  "sierra",
  "tango",
  "uniform",
  "victor",
  "whiskey",
  "xray",
  "yankee",
  "zulu"]