import Data.Char (ord)
import Data.List (scanr1)

part1 :: [Int] -> Int
part1 bank =
  let mx = scanr1 max bank
   in maximum [10 * a + b | (a, b) <- zip bank (tail mx)]

part2 :: [Int] -> Int
part2 bank =
  let ds = take 12 . reverse $ go (length bank - 12) [] bank
   in foldl (\acc d -> acc * 10 + fromIntegral d) 0 ds
  where
    go rem stack [] = stack
    go rem stack (x : xs)
      | rem > 0
          && not (null stack)
          && head stack < x =
          go (rem - 1) (tail stack) (x : xs)
      | otherwise = go rem (x : stack) xs

main :: IO ()
main = do
  contents <- getContents
  let banks = map (map (\c -> ord c - 48)) (lines contents)
  print $ sum $ map part1 banks
  print $ sum $ map part2 banks
