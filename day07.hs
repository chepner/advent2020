import System.Environment
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M

type Color = String

digit :: ReadP Char
digit = satisfy isDigit

number :: ReadP Int
number = read <$> many1 digit

letter :: ReadP Char
letter = satisfy isAlpha

letters :: ReadP String
letters = many1 letter

color :: ReadP String
color = do adj <- letters
           skipSpaces
           name <- letters
           return $ adj ++  " " ++ name

contents :: ReadP (Color, Int)
contents = do count <- number
              skipSpaces
              name <- color
              skipSpaces
              string "bag"
              option "" (string "s")
              return (name, count)

nocontents :: ReadP [(Color, Int)]
nocontents = string "no other bags" >> return []

-- Maps a color with the colors that can directly contain it
type Graph = M.Map Color [Color]

rule :: ReadP Graph
rule = do outer <- color
          string " bags contain"
          skipSpaces
          list <- choice [nocontents, sepBy1 contents (string ", ")]
          string "."
          return $ M.fromList [ (fst x, [outer]) | x <- list]


-- Graph search to find all colors that can transitively contain a given color
getContainers :: Graph -> Color -> [Color]
getContainers g c = let direct = fromMaybe [] (M.lookup c g)
                    in direct ++ concatMap (getContainers g) direct

main = do
   args <- getArgs
   let fname = args !! 0
   text_rules <- lines <$> readFile fname

   let graph = M.unionsWith (++) $ map (fst . head . readP_to_S rule) text_rules

   print $ length $ nub $ getContainers graph "shiny gold"
