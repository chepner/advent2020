{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Text.ParserCombinators.ReadP
import Data.Function
import Data.Char
import qualified Data.Map as M
import Data.Foldable (asum, traverse_)
import Data.Maybe
import Control.Applicative (liftA2)

-- Command-line boilerplate
data Options = Options {
    fileName :: FilePath
}

options :: OA.Parser Options
options = Options
          <$> OA.argument OA.str (OA.metavar "FILE")

p = OA.info (options OA.<**> OA.helper)
    ( OA.fullDesc
      <> OA.progDesc "Solve Day 19 for Advent of Code 2020"
      <> OA.header "Advent 2020: Day 19" )
-- End command-line boilerplate


-- | Assume that readS_to_P will produce a result like
--   [(a,"")], and just get back the a
runDParser :: ReadP a -> String -> a
runDParser p s = readP_to_S p s & (!! 0) & fst

buildTerminal :: ReadP (ReadP String)
buildTerminal = do
     skipSpaces
     c <- between (char '"') (char '"') (many1 get)
     return (string c)

-- This needs to interact with the final parser, to recurively
-- expand the rule
buildNonterminal = return $ many1 (satisfy isDigit)

buildAlternative = do
     skipSpaces
     possibilities <- sepBy1 buildConcat (skipSpaces >> char '|' >> skipSpaces) <* eof
     return $ asum possibilities
     
buildConcat = do
  skipSpaces
  t <- (buildTerminal <++ buildNonterminal)
  skipSpaces
  ts <- buildConcat
  return (liftA2 (++) t ts)

buildRule = do
   ruleNum <- read <$> many (satisfy isDigit)
   char ':'
   skipSpaces
   rule <- buildAlternative
   return (ruleNum :: Int, rule)

parseRule s = runDParser buildRule s

parseGrammar :: [String] -> M.Map Int (ReadP String)
parseGrammar = M.fromList . map parseRule

readInput :: FilePath -> IO ([String], [String])
readInput fname = do
    contents <- lines <$> readFile fname
    let (grammar, input) = break null contents
    return (grammar, dropWhile null input)

getRule :: Int -> M.Map Int (ReadP String) -> ReadP String
getRule n m = fromJust (M.lookup n m)

main = do
   args <- OA.execParser p
   (grammar, input) <- readInput (fileName args)
   let parser = parseGrammar grammar
       r = getRule 0 parser

   putStrLn "Grammar"
   putStrLn "======="
   traverse_ putStrLn grammar

   putStrLn "\nInput"
   putStrLn "====="
   traverse_ putStrLn input

   putStrLn "Part a"
   
   traverse (print . runDParser r) (tail input)

   putStrLn "Part b"
