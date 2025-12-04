import Data.List (foldl', permutations)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.String (Parser)

parse_ :: String -> (String, String, Int)
parse_ = either (error . show) id . parse parser ""
  where
    parser :: Parser (String, String, Int)
    parser = do
      a <- many1 letter
      spaces
      string "to"
      spaces
      b <- many1 letter
      spaces
      char '='
      spaces
      d <- read <$> many1 digit
      return (a, b, d)

type Graph = M.Map String [(String, Int)]

mkGraph :: [(String, String, Int)] -> Graph
mkGraph = foldl' go M.empty
  where
    go g (a, b, d) =
      M.alter (Just . ((b, d) :) . fromMaybe []) a $
        M.alter (Just . ((a, d) :) . fromMaybe []) b g

distance :: Graph -> [String] -> Maybe Int
distance g = go 0
  where
    go acc [] = Just acc
    go acc [_] = Just acc
    go acc (x : y : zs) = do
      d <- lookup y =<< M.lookup x g
      go (acc + d) (y : zs)

part1 :: Graph -> Int
part1 g = minimum $ map (fromMaybe maxBound . distance g) (permutations (M.keys g))

part2 :: Graph -> Int
part2 g = maximum $ map (fromMaybe maxBound . distance g) (permutations (M.keys g))

main :: IO ()
main = do
  edges <- map parse_ . lines <$> getContents
  let graph = mkGraph edges
  print $ part1 graph
  print $ part2 graph
