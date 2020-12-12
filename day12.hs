{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Text.ParserCombinators.ReadP
import Data.List (sort, tails, foldl')
import Data.Char
import Data.Function ((&))
import Data.Function.Memoize

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

data Heading = North | South | East | West | Forward | TurnLeft | TurnRight deriving Show

data NavPair = NP { newHeading :: Heading, value :: Int } deriving Show

navpair :: ReadP NavPair
navpair = do
    d <- satisfy (`elem` "NSEWFLR")
    n <- many1(satisfy isDigit)
    eof
    return $ NP (case d of
               'N' -> North
               'S' -> South
               'E' -> East
               'W' -> West
               'F' -> Forward
               'L' -> TurnLeft
               'R' -> TurnRight) (read n)

data Position = P { heading:: Heading, posX, posY :: Int } deriving Show

initial :: Position
initial = P East 0 0

forward :: Heading -> Heading
forward = id

turnLeft, turnRight :: Heading -> Heading
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North


-- Might be an opportunity to play with lenses
move :: NavPair -> Position -> Position
move (NP North v) p = p { posY = posY p + v }
move (NP South v) p = p { posY = posY p - v }
move (NP West v) p = p { posX = posX p - v }
move (NP East v) p = p { posX = posX p + v }
move nav p = let h = heading p
             in case newHeading nav of
                  Forward -> move (nav {newHeading = forward h}) p
                  TurnLeft -> p { heading = turnLeft h}
                  TurnRight -> p { heading = turnRight h}

manhatten :: Position -> Position -> Int
manhatten (P _ x2 y2) (P _ x1 y1) = abs (x2 - x1) + abs (y2 - y1)

-- readNavPairs :: FilePath -> IO [NavPair]
readNavPairs fname = do
    contents <- lines <$> readFile fname
    return $ map (fst . (!! 0) . readP_to_S navpair) contents 


main = do
   args <- OA.execParser p
   navpairs <- readNavPairs (fileName args)

   putStrLn "Part a"
   print $ manhatten (foldl' (flip move) initial navpairs) initial

   putStrLn "Part b"
