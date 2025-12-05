import Data.List (foldl', group, nub)

-- encode Int to base-26 String
enc :: Int -> String
enc 0 = "a"
enc n = reverse $ go n
  where
    go 0 = []
    go x =
      let (q, r) = x `divMod` 26
       in toEnum (fromEnum 'a' + r) : go q

-- decode base-26 String to Int
dec :: String -> Int
dec = foldl' (\acc x -> acc * 26 + (fromEnum x - fromEnum 'a')) 0

-- return next valid string satisfying all predicates
next :: [String -> Bool] -> String -> String
next = go
  where
    go preds s =
      let s' = enc (dec s + 1)
       in if all ($ s') preds
            then s'
            else go preds s'

part1 :: String -> String
part1 = next [r1, r2, r3]
  where
    r1 = any (\(a, b, c) -> succ a == b && succ b == c) . (\s -> zip3 s (tail s) (drop 2 s))
    r2 = all (`notElem` "iol")
    r3 = (>= 2) . length . nub . map head . filter ((>= 2) . length) . group

part2 :: String -> String
part2 = part1 . part1

main :: IO ()
main = do
  line <- getLine
  putStrLn $ part1 line
  putStrLn $ part2 line
