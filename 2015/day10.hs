say :: String -> String
say [] = ""
say [x] = "1" ++ [x]
say (x : xs) = go 1 x xs
  where
    go n c [] = show n ++ [c]
    go n c (y : ys)
      | c == y = go (n + 1) c ys
      | otherwise = show n ++ [c] ++ go 1 y ys

part1 :: String -> Int
part1 s = length $ iterate say s !! 40

part2 :: String -> Int
part2 s = length $ iterate say s !! 50

main :: IO ()
main = do
  line <- getLine
  print $ part1 line
  print $ part2 line
