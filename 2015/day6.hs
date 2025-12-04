import Control.Monad
import Control.Monad.ST
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Text.Parsec
import Text.Parsec.String (Parser)

data Op = Up | Down | Toggle

data Cmd = Cmd {op :: Op, a :: (Int, Int), b :: (Int, Int)}

parse' :: String -> Cmd
parse' = either (error . show) id . parse parser ""
  where
    parser :: Parser Cmd
    parser = do
      op <-
        (try (string "turn on") >> return Up)
          <|> (try (string "turn off") >> return Down)
          <|> (string "toggle" >> return Toggle)
      spaces
      x1 <- read <$> many1 digit
      char ','
      y1 <- read <$> many1 digit
      spaces
      string "through"
      spaces
      x2 <- read <$> many1 digit
      char ','
      y2 <- read <$> many1 digit
      return $ Cmd op (x1, y1) (x2, y2)

type Grid s = VUM.MVector s Int

solve :: (Op -> Int -> Int) -> [Cmd] -> Int
solve f cmds = runST $ do
  grid <- VUM.replicate (1000 * 1000) 0
  forM_ cmds $ \(Cmd op (x1, y1) (x2, y2)) -> do
    forM_ [y1 .. y2] $ \y ->
      forM_ [x1 .. x2] $ \x -> do
        let idx = y * 1000 + x
        VUM.modify grid (f op) idx
  VUM.foldl' (+) 0 grid

part1 :: [Cmd] -> Int
part1 = solve f
  where
    f Up _ = 1
    f Down _ = 0
    f Toggle v = if v == 0 then 1 else 0

part2 :: [Cmd] -> Int
part2 = solve f
  where
    f Up v = v + 1
    f Down v = max 0 (v - 1)
    f Toggle v = v + 2

main :: IO ()
main = do
  cmds <- map parse' . lines <$> getContents
  print $ part1 cmds
  print $ part2 cmds
