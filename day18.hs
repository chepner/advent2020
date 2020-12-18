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
          n <- skipSpaces >> many1 (satisfy isDigit)
          return (IntVal (read n))

group :: ReadP Expr
group = do
         skipSpaces
         char '('
         skipSpaces
         e <- expr
         skipSpaces
         char ')'
         return e

op :: ReadP (Expr -> Expr -> Expr)
op = (char '+' >> return (Op Add)) <++ (char '*' >> return (Op Mult))

expr :: ReadP Expr
expr = chainl1 (number <++ group) (skipSpaces >> op)

fullExpr :: ReadP Expr
fullExpr = expr <* (skipSpaces >> eof)
         

parseExpr :: String -> Expr
parseExpr = fst . (!! 0) . readP_to_S fullExpr

main = do
   args <- OA.execParser p

   putStrLn "Part a"
   contents <- lines <$> readFile (fileName args)
   print $ sum (map (eval . parseExpr) contents)

   putStrLn "Part b"
