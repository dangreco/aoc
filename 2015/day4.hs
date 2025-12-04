import Data.Hash.MD5

test :: String -> Int -> Int -> Bool
test s n m =
  let h = md5s (Str (s ++ show n))
   in take m h == replicate m '0'

part1 :: String -> Int
part1 s = head [n | n <- [0 ..], test s n 5]

part2 :: String -> Int
part2 s = head [n | n <- [0 ..], test s n 6]

main :: IO ()
main = do
  line <- getLine
  print $ part1 line
  print $ part2 line
