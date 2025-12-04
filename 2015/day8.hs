import Data.Char (isHexDigit)

lengthCode :: String -> Int
lengthCode = length

lengthMemory :: String -> Int
lengthMemory = go 0
  where
    go n [] = n
    go n ('\\' : '\\' : rest) = go (n + 1) rest
    go n ('\\' : '"' : rest) = go (n + 1) rest
    go n ('\\' : 'x' : a : b : rest) | isHexDigit a && isHexDigit b = go (n + 1) rest
    go n ('"' : rest) = go n rest
    go n (_ : rest) = go (n + 1) rest

lengthExpanded :: String -> Int
lengthExpanded s = 2 + go 0 s
  where
    go n [] = n
    go n ('"' : rest) = go (n + 2) rest
    go n ('\\' : rest) = go (n + 2) rest
    go n (_ : rest) = go (n + 1) rest

part1 :: [String] -> Int
part1 lines = sum (map lengthCode lines) - sum (map lengthMemory lines)

part2 :: [String] -> Int
part2 lines = sum (map lengthExpanded lines) - sum (map lengthCode lines)

main :: IO ()
main = do
  lines <- lines <$> getContents
  print $ part1 lines
  print $ part2 lines
