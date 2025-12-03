import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)

parse :: [Char] -> [(Int, Int)]
parse s = case Text.Parsec.parse ranges "" s of
  Right ranges -> ranges
  _ -> error "invalid input"
  where
    ranges = range `sepBy` char ','
    range = do
      lo <- many1 digit
      char '-'
      hi <- many1 digit
      return (read lo, read hi)

sieve :: (Int -> Bool) -> [(Int, Int)] -> [Int]
sieve p = concatMap (\(lo, hi) -> filter p [lo .. hi])

-- wildly inefficient
part1 :: Int -> Bool
part1 x =
  let s = show x
      len = length s
   in even len
        && any
          ( \k ->
              let (a, b) = splitAt k s
               in not (null a) && a == b
          )
          [1 .. len `div` 2]

-- wildly inefficient
part2 :: Int -> Bool
part2 x =
  let s = show x
      len = length s
   in any
        ( \k ->
            len `mod` k == 0
              && let block = take k s
                     reps = len `div` k
                  in reps >= 2 && concat (replicate reps block) == s
        )
        [1 .. len `div` 2]

main :: IO ()
main = do
  line <- getLine
  let ranges = Main.parse line
  print $ sum $ sieve part1 ranges
  print $ sum $ sieve part2 ranges
