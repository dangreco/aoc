parse :: [Char] -> Int
parse ('L' : x) = negate (read x)
parse ('R' : x) = read x
parse _ = error "invalid input"

part1 :: [Int] -> Int -> Int
part1 deltas pos = go deltas pos 0
  where
    go [] _ acc = acc
    go (d : ds) pos acc =
      let pos' = (pos + d) `mod` 100
          acc' = if pos' == 0 then acc + 1 else acc
       in go ds pos' acc'

part2 :: [Int] -> Int -> Int
part2 deltas pos = go deltas pos 0
  where
    go [] _ acc = acc
    go (d : ds) pos acc =
      let pos' = (pos + d) `mod` 100
          acc' = acc + clicks d pos
       in go ds pos' acc'

    clicks delta pos
      | delta == 0 = 0
      | otherwise = part1 (replicate (abs delta) (signum delta)) pos

main :: IO ()
main = do
  contents <- getContents
  let deltas = map parse (lines contents)
  print $ part1 deltas 50
  print $ part2 deltas 50
