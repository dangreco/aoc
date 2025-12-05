import Data.List (sortBy)
import Text.Parsec
import Text.Parsec.String (Parser)

type Range = (Int, Int)

parseRange :: String -> Range
parseRange = either (error . show) id . parse parser ""
  where
    parser :: Parser Range
    parser = do
      a <- read <$> many1 digit
      char '-'
      b <- read <$> many1 digit
      return (a, b)

part1 :: [Range] -> [Int] -> Int
part1 ranges ids = length $ filter (isFresh ranges) ids
  where
    isFresh ranges id = any (\(a, b) -> a <= id && id <= b) ranges

part2 :: [Range] -> Int
part2 = sum . map (\(a, b) -> b - a + 1) . merge
  where
    merge = foldr insert [] . sortBy (\(a, b) (c, d) -> compare a c)
    insert r [] = [r]
    insert (a, b) ((c, d) : rs)
      | b < c = (a, b) : (c, d) : rs
      | a > d = (c, d) : insert (a, b) rs
      | otherwise = insert (min a c, max b d) rs

main = do
  (l, r) <- fmap (break null . lines) getContents
  let ranges = map parseRange l
      ids = map read (tail r) :: [Int]
  print $ part1 ranges ids
  print $ part2 ranges
