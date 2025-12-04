import Control.Monad.State
import Data.Bits (complement, shiftL, shiftR, (.&.), (.|.))
import Data.Map.Strict qualified as M
import Data.Word (Word16)
import Text.Parsec (digit, letter, many1, parse, spaces, string, try, (<|>))
import Text.Parsec.String (Parser)

data Expr
  = Val Word16
  | Wire String
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | LShift Expr Int
  | RShift Expr Int

parse_ :: String -> (String, Expr)
parse_ = either (error . show) id . parse parser ""
  where
    parser :: Parser (String, Expr)
    parser = do
      expr <- parseExpr
      spaces
      string "->"
      spaces
      wire <- many1 letter
      return (wire, expr)

    parseExpr :: Parser Expr
    parseExpr =
      try parseAnd
        <|> try parseOr
        <|> try parseLShift
        <|> try parseRShift
        <|> try parseNot
        <|> parseValueOrWire

    parseAnd :: Parser Expr
    parseAnd = do
      e1 <- parseValueOrWire
      spaces
      string "AND"
      spaces
      e2 <- parseValueOrWire
      return $ And e1 e2

    parseOr :: Parser Expr
    parseOr = do
      e1 <- parseValueOrWire
      spaces
      string "OR"
      spaces
      e2 <- parseValueOrWire
      return $ Or e1 e2

    parseLShift :: Parser Expr
    parseLShift = do
      e <- parseValueOrWire
      spaces
      string "LSHIFT"
      spaces
      n <- read <$> many1 digit
      return $ LShift e n

    parseRShift :: Parser Expr
    parseRShift = do
      e <- parseValueOrWire
      spaces
      string "RSHIFT"
      spaces
      n <- read <$> many1 digit
      return $ RShift e n

    parseNot :: Parser Expr
    parseNot = do
      string "NOT"
      spaces
      e <- parseValueOrWire
      return $ Not e

    parseValueOrWire :: Parser Expr
    parseValueOrWire =
      try (Val . read <$> many1 digit)
        <|> (Wire <$> many1 letter)

eval :: Expr -> State (M.Map String Expr) Word16
eval (Val v) = return v
eval (Wire w) = do
  circuit <- get
  case M.lookup w circuit of
    Just expr -> do
      v <- eval expr
      modify (M.insert w (Val v))
      return v
    _ -> error ""
eval (And e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 .&. v2)
eval (Or e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1 .|. v2)
eval (Not e) = do
  v <- eval e
  return (complement v)
eval (LShift e n) = do
  v <- eval e
  return (v `shiftL` n)
eval (RShift e n) = do
  v <- eval e
  return (v `shiftR` n)

part1 :: M.Map String Expr -> Word16
part1 = evalState (eval (Wire "a"))

part2 :: M.Map String Expr -> Word16
part2 circuit =
  let a = evalState (eval (Wire "a")) circuit
      circuit' = M.insert "b" (Val a) circuit
   in evalState (eval (Wire "a")) circuit'

main :: IO ()
main = do
  lines <- lines <$> getContents
  let circuit = M.fromList $ map parse_ lines
  print $ part1 circuit
  print $ part2 circuit
