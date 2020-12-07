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
number = read <$> many1 (satisfy isDigit)

letters :: ReadP String
letters = many1 (satisfy isAlpha)

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
type GraphA = M.Map Color [Color]
type GraphB = M.Map Color [(Color, Int)]

rule :: ReadP (Color, [(Color, Int)])
rule = do outer <- color
          string " bags contain"
          skipSpaces
          list <- choice [nocontents, sepBy1 contents (string ", ")]
          string "."
          return (outer, list)
          -- return $ M.fromList [ (fst x, [outer]) | x <- list]


-- Graph search to find all colors that can transitively contain a given color
getContainers :: GraphA -> Color -> [Color]
getContainers g c = let direct = fromMaybe [] (M.lookup c g)
                    in direct ++ concatMap (getContainers g) direct


getContained :: GraphB -> Color -> Int
getContained g c = let contents = fromMaybe [] (M.lookup c g)
                       colors = map fst contents
                       counts = map snd contents
                       contained = sum ( zipWith (*) (map (getContained g) colors) counts)
                   in sum counts + contained


main = do
   args <- getArgs
   let fname = args !! 0
   text_rules <- lines <$> readFile fname

   let rules = map (fst . head . readP_to_S rule) text_rules

   let alistPartA = map (\(c, cs) -> [(x, [c]) | (x,_) <- cs]) rules
   let graphPartA = M.unionsWith (++) $ map M.fromList alistPartA

   print "Part a"
   print $ length $ nub $ getContainers graphPartA "shiny gold"

   print "Part b"
   let graphPartB = M.fromList rules
   print $ getContained graphPartB "shiny gold"

