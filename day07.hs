{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M

newtype Color = Color String deriving (Eq, Ord)

number :: ReadP Int
number = read <$> many1 (satisfy isDigit)

letters :: ReadP String
letters = many1 (satisfy isAlpha)

color :: ReadP Color
color = do adj <- letters
           skipSpaces
           name <- letters
           return $ Color $ adj ++ " " ++ name

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

totalcontents :: ReadP [(Color, Int)]
totalcontents = nocontents +++ sepBy1 contents (string ", ")

rule :: ReadP (Color, [(Color, Int)])
rule = do outer <- color
          string " bags contain"
          skipSpaces
          list <- totalcontents
          string "."
          return (outer, list)

-- Maps a color with the colors that can directly contain it
type GraphA = M.Map Color [Color]
type GraphB = M.Map Color [(Color, Int)]

-- Graph search to find all colors that can transitively contain a given color
getContainers :: GraphA -> Color -> [Color]
getContainers g c = let direct = fromMaybe [] (M.lookup c g)
                    in direct ++ concatMap (getContainers g) direct

-- Recursively compute the weight of a single bag, based on the weights
-- of the required contents
getContained :: GraphB -> Color -> Int
getContained g c = let contents = fromMaybe [] (M.lookup c g)
                       colors = map fst contents
                       counts = map snd contents
                       contained = sum ( zipWith (*) (map (getContained g) colors) counts)
                   in sum counts + contained


newtype Options = Options {
  fileName :: FilePath
}

options :: OA.Parser Options
options = Options <$> OA.argument OA.str (OA.metavar "FILE")

p = OA.info (options OA.<**> OA.helper)
    ( OA.fullDesc
      <> OA.progDesc "Solve Day 7 for Advent of Code 2020"
      <> OA.header "Advent 2020: Day 7" )

main = do
   args <- OA.execParser p

   text_rules <- lines <$> readFile (fileName args)

   let rules = map (fst . head . readP_to_S rule) text_rules

   let alistPartA = map (\(c, cs) -> [(x, [c]) | (x,_) <- cs]) rules
   let graphPartA = M.unionsWith (++) $ map M.fromList alistPartA

   let myBagColor = Color "shiny gold"

   print "Part a"
   print $ length $ nub $ getContainers graphPartA myBagColor

   print "Part b"
   let graphPartB = M.fromList rules
   print $ getContained graphPartB myBagColor

