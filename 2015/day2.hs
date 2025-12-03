import Data.List (sort)
import Text.Parsec
import Text.Parsec.String (Parser)

type Dim = (Int, Int, Int)

parse_ :: String -> Dim
parse_ s = either (error "input") id $ parse dims "" s
  where
    dims = do
      l <- many1 digit
      char 'x'
      w <- many1 digit
      char 'x'
      h <- many1 digit
      return (read l, read w, read h)

part1 :: [Dim] -> Int
part1 = sum . map solve
  where
    solve (l, w, h) =
      let area = 2 * l * w + 2 * w * h + 2 * h * l
          [x, y, _] = sort [l, w, h]
       in area + x * y

part2 :: [Dim] -> Int
part2 = sum . map solve
  where
    solve (l, w, h) =
      let vol = l * w * h
          [x, y, _] = sort [l, w, h]
       in vol + 2 * (x + y)

main :: IO ()
main = do
  contents <- getContents
  let dims = map parse_ (lines contents)
  print $ part1 dims
  print $ part2 dims
