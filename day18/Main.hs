module Main where

import Data.Char (digitToInt)
import Data.List (uncons)
import Data.Maybe (mapMaybe)

data Operation = Plus | Mul deriving (Eq, Show)

data Token = Num Int | Op Operation | LParen | RParen deriving (Eq, Show)

data AST = Leaf Int | Expr AST | Sum AST AST | Mult AST AST deriving (Eq, Show)

type Parser = [Token] -> (AST, [Token])

astOp :: Operation -> (AST -> AST -> AST)
astOp Plus = Sum
astOp Mul = Mult

-- Will get sad with double digit nums
charToToken :: Char -> Maybe Token
charToToken ' ' = Nothing
charToToken '+' = Just $ Op Plus
charToToken '*' = Just $ Op Mul
charToToken '(' = Just LParen
charToToken ')' = Just RParen
charToToken n = Just $ Num (digitToInt n)

tokenize :: String -> [Token]
tokenize = mapMaybe charToToken

parseExpr :: Parser -> Parser
parseExpr p ((Num n) : ts) = (Leaf n, ts)
parseExpr p (LParen : ts) = case p ts of
  (exprAst, RParen : rest) -> (Expr exprAst, rest)
  _ -> error "error parsing expression"
parseExpr p t = error ("expecting num got" ++ show t)

parseExprP1 :: Parser
parseExprP1 = parseExpr parseAstHelperP1

parseAstHelperP1 :: Parser
parseAstHelperP1 ts =
  let (f1, ts1) = parseExprP1 ts
   in go f1 ts1
  where
    go acc ((Op op) : ts2) = let (f2, ts3) = parseExprP1 ts2 in go (astOp op acc f2) ts3
    go acc rest = (acc, rest)

parseAstP1 :: [Token] -> AST
parseAstP1 t = case parseAstHelperP1 t of
  (a, []) -> a
  _ -> error "did not fully parse tokens"

parseExprP2 :: Parser
parseExprP2 = parseExpr parseAstHelperP2

parsePlus :: Parser
parsePlus ts =
  let (f1, ts1) = parseExprP2 ts
   in go f1 ts1
  where
    go acc ((Op Plus) : ts2) = let (f2, ts3) = parseExprP2 ts2 in go (Sum acc f2) ts3
    go acc rest = (acc, rest)

parseAstHelperP2 :: Parser
parseAstHelperP2 ts =
  let (f1, ts1) = parsePlus ts
   in go f1 ts1
  where
    go acc ((Op op) : ts2) = let (f2, ts3) = parsePlus ts2 in go (astOp op acc f2) ts3
    go acc rest = (acc, rest)

parseAstP2 :: [Token] -> AST
parseAstP2 t = case parseAstHelperP2 t of
  (a, []) -> a
  _ -> error "did not fully parse tokens"

eval :: AST -> Int
eval (Expr a) = eval a
eval (Leaf n) = n
eval (Sum a b) = eval a + eval b
eval (Mult a b) = eval a * eval b

part1 :: IO ()
part1 = do
  input <- getInput
  print $ sum $ map (eval . parseAstP1 . tokenize) input
  return ()

part2 :: IO ()
part2 = do
  input <- getInput
  print $ sum $ map (eval . parseAstP2 . tokenize) input
  return ()

main :: IO ()
main = part2

getInput :: IO [String]
getInput = lines <$> readFile "./in"
