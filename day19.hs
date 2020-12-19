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
runDParser :: ReadP a -> String -> Maybe a
runDParser p s = case readP_to_S p s of
                    [(x, _)] -> Just x
                    otherwise -> Nothing

buildTerminal :: ReadP (ReadP String)
buildTerminal = do
     skipSpaces
     c <- between (char '"') (char '"') (many1 get)
     return (string c)

-- This needs to interact with the final parser, to recurively
-- expand the rule
buildNonterminal :: Grammar -> ReadP (ReadP String)
buildNonterminal g = do
   ruleNum <- read <$> many1 (satisfy isDigit)
   return (getRule ruleNum g)

buildAlternative :: Grammar -> ReadP (ReadP String)
buildAlternative g = do
     skipSpaces
     possibilities <- sepBy1 (buildConcat g) (skipSpaces >> char '|' >> skipSpaces) <* eof
     return $ asum possibilities
     
buildConcat :: Grammar -> ReadP (ReadP String)
buildConcat g = do
  skipSpaces
  t <- (buildTerminal <++ (buildNonterminal g))
  skipSpaces
  ts <- option (pure []) (buildConcat g)
  return (liftA2 (++) t ts)

buildRule :: Grammar -> ReadP (Int, ReadP String)
buildRule g = do
   ruleNum <- read <$> many (satisfy isDigit)
   char ':'
   skipSpaces
   rule <- buildAlternative g
   return (ruleNum, rule)

type Grammar = M.Map Int (ReadP String)

parseRule :: Grammar -> String -> (Int, ReadP String)
parseRule g s = fromJust $ runDParser (buildRule g) s

-- Does... does this actually work? Building the grammar in terms of itself?
-- It compiles, but parsing seems to be failing; that could be a problem elsewhere...
parseGrammar :: [String] -> M.Map Int (ReadP String)
parseGrammar s = let g = M.fromList ( map (parseRule g) s ) in g

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
   let results = map (runDParser (r <* eof)) input
   print $ length $ catMaybes results
   
   -- traverse (print . readP_to_S (r <* eof)) input


   putStrLn "Part b"
