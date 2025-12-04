import Data.Set (Set)
import Data.Set qualified as Set

parse :: [String] -> Set (Int, Int)
parse s =
  let h = length s
      w = length (head s)
   in Set.fromList
        [ (r, c)
          | r <- [0 .. h - 1],
            c <- [0 .. w - 1],
            (s !! r) !! c == '@'
        ]

isAccessible :: Set (Int, Int) -> (Int, Int) -> Bool
isAccessible rolls (r, c) = count < 4
  where
    neighbors = [(r + dr, c + dc) | dr <- [-1 .. 1], dc <- [-1 .. 1], (dr, dc) /= (0, 0)]
    count = length $ filter (`Set.member` rolls) neighbors

toRemove :: Set (Int, Int) -> Set (Int, Int)
toRemove rolls = Set.fromList [pos | pos <- Set.toList rolls, isAccessible rolls pos]

part1 :: Set (Int, Int) -> Int
part1 rolls = Set.size (toRemove rolls)

part2 :: Set (Int, Int) -> Int
part2 rolls = go rolls 0
  where
    go current count
      | Set.null toRem = count
      | otherwise = go (Set.difference current toRem) (count + Set.size toRem)
      where
        toRem = toRemove current

main :: IO ()
main = do
  contents <- getContents
  let grid = parse (lines contents)
  print $ part1 grid
  print $ part2 grid
