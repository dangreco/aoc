part1 :: String -> Int
part1 = sum . map (\c -> if c == '(' then 1 else -1)

part2 :: String -> Int
part2 = go 1 0
  where
    go _ _ [] = error "invalid input"
    go i f (c : cs)
      | f' == -1 = i
      | otherwise = go (i + 1) f' cs
      where
        f' = f + if c == '(' then 1 else -1

main :: IO ()
main = do
  line <- getLine
  print $ part1 line
  print $ part2 line
