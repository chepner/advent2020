{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Text.ParserCombinators.ReadP
import Data.Char
import Debug.Trace

-- Command-line boilerplate
data Options = Options {
    fileName :: FilePath
}

options :: OA.Parser Options
options = Options
          <$> OA.argument OA.str (OA.metavar "FILE")

p = OA.info (options OA.<**> OA.helper)
    ( OA.fullDesc
      <> OA.progDesc "Solve Day 10 for Advent of Code 2020"
      <> OA.header "Advent 2020: Day 10" )
-- End command-line boilerplate

data Op = Add | Mult deriving Show
        
data Expr = IntVal Integer
          | Op Op Expr Expr deriving Show

eval :: Expr -> Integer
eval (IntVal x) = x
eval (Op Add x y) = eval x + eval y
eval (Op Mult x y) = eval x * eval y

number :: ReadP Expr
number = do
          skipSpaces
          n <- many1 (satisfy isDigit)
          return (IntVal (read n))

group :: ReadP Expr -> ReadP Expr
group p = between
           (skipSpaces >> char '(')
           (skipSpaces >> char ')')
           (skipSpaces >> p)

op :: ReadP (Expr -> Expr -> Expr)
op = addOp <++ multOp

addOp :: ReadP (Expr -> Expr -> Expr)
addOp = do
          skipSpaces
          char '+'
          return (Op Add)

multOp :: ReadP (Expr -> Expr -> Expr)
multOp = do
          skipSpaces
          char '*'
          return (Op Mult)

term :: ReadP Expr ->  ReadP Expr
term e = number <++ group e

expr :: ReadP Expr
expr = chainl1 (term expr) op where

exprB :: ReadP Expr
exprB = chainl1 factor multOp

factor :: ReadP Expr
factor = chainl1 (term exprB) addOp

fullExpr :: ReadP Expr -> ReadP Expr
fullExpr = (<* (skipSpaces >> eof))

parseExpr :: ReadP Expr -> String -> Expr
parseExpr p = fst . (!! 0) . readP_to_S (fullExpr p)

main = do
   args <- OA.execParser p
   contents <- lines <$> readFile (fileName args)

   putStrLn "Part a"
   print $ sum (map (eval . parseExpr expr) contents)

   putStrLn "Part b"
   print $ sum (map (eval . parseExpr exprB) contents)
