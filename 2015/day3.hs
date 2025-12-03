import Data.Set qualified as Set

type Seen = Set.Set (Int, Int)

move :: (Int, Int) -> Char -> (Int, Int)
move (x, y) '^' = (x, y + 1)
move (x, y) 'v' = (x, y - 1)
move (x, y) '>' = (x + 1, y)
move (x, y) '<' = (x - 1, y)
move _ _ = error "invalid input"

part1 :: String -> Int
part1 line = length $ go Set.empty (0, 0) line
  where
    go seen _ [] = seen
    go seen p (c : cs) = go (Set.insert p seen) (move p c) cs

part2 :: String -> Int
part2 line = length $ go Set.empty (0, 0) (0, 0) True line
  where
    go seen _ _ _ [] = seen
    go seen ps pr True (c : cs) = go (Set.insert ps seen) (move ps c) pr False cs
    go seen ps pr False (c : cs) = go (Set.insert pr seen) ps (move pr c) True cs

main :: IO ()
main = do
  line <- getLine
  print $ part1 line
  print $ part2 line
