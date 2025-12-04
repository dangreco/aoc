import Data.List (isInfixOf)

solve :: [String] -> (String -> Bool) -> Int
solve strings predicate = length $ filter predicate strings

part1 :: String -> Bool
part1 s = all ($ s) [r1, r2, r3]
  where
    r1 s = length (filter (`elem` "aeiou") s) >= 3
    r2 s = any (uncurry (==)) $ zip s (tail s)
    r3 s = not $ any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]

part2 :: String -> Bool
part2 s = all ($ s) [r1, r2]
  where
    r1 s = any hasPair (zip [0 ..] (zip s (tail s)))
      where
        hasPair (i, pair@(a, b)) =
          let rest = drop (i + 2) s
           in pair `elem` zip rest (tail rest)
    r2 s = any (\(a, _, c) -> a == c) $ zip3 s (tail s) (drop 2 s)

main :: IO ()
main = do
  contents <- getContents
  let strings = lines contents
  print $ solve strings part1
  print $ solve strings part2
