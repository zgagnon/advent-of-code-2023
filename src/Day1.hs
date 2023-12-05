import           Data.Foldable
import           Data.Function
import           Data.List       (isPrefixOf, isSuffixOf)
import           Data.List.Split
import           Debug.Trace
import           System.IO


front :: String -> Int
front x = if isPrefixOf "one" x then 1
  else if isPrefixOf "two" x then 2
  else if isPrefixOf "three" x then 3
  else if isPrefixOf "four" x then 4
  else if isPrefixOf "five" x then 5
  else if isPrefixOf "six" x then 6
  else if isPrefixOf "seven" x then 7
  else if isPrefixOf "eight" x then 8
  else if isPrefixOf "nine" x then 9
  else if isPrefixOf "1" x then 1
  else if isPrefixOf "2" x then 2
  else if isPrefixOf "3" x then 3
  else if isPrefixOf "4" x then 4
  else if isPrefixOf "5" x then 5
  else if isPrefixOf "6" x then 6
  else if isPrefixOf "7" x then 7
  else if isPrefixOf "8" x then 8
  else if isPrefixOf "9" x then 9
  else front $ tail x

back :: String -> Int
back x = if isSuffixOf "one" x then 1
  else if isSuffixOf "two" x then 2
  else if isSuffixOf "three" x then 3
  else if isSuffixOf "four" x then 4
  else if isSuffixOf "five" x then 5
  else if isSuffixOf "six" x then 6
  else if isSuffixOf "seven" x then 7
  else if isSuffixOf "eight" x then 8
  else if isSuffixOf "nine" x then 9
  else if isSuffixOf "1" x then 1
  else if isSuffixOf "2" x then 2
  else if isSuffixOf "3" x then 3
  else if isSuffixOf "4" x then 4
  else if isSuffixOf "5" x then 5
  else if isSuffixOf "6" x then 6
  else if isSuffixOf "7" x then 7
  else if isSuffixOf "8" x then 8
  else if isSuffixOf "9" x then 9
  else back (reverse (tail (reverse x)))

dumbpair :: String -> Int
dumbpair x = let
  f = front x
  b = back x
  in trace (x ++ " " ++ (show f) ++ " " ++ (show b)) (f * 10 + b)

main :: IO ()
main = do
  contents <- readFile "./inputs/day-1.input"
  let split = splitOn "\n" contents
  let parsed = map dumbpair split
  let sums = sum parsed
  print parsed
  print (length parsed)
  print sums
