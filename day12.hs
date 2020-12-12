{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Text.ParserCombinators.ReadP
import Data.List (sort, tails, foldl')
import Data.Char
import Data.Function ((&))
import Data.Function.Memoize
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


turnLeft, turnRight :: Int -> Heading -> Heading
turnLeft 0 d = d
turnLeft v d = turnLeft (v - 90) (rotate90 d)
  where rotate90 North = West
        rotate90 West = South
        rotate90 South = East
        rotate90 East = North

turnRight 0 d = d
turnRight v d = turnRight (v - 90) (rotate90 d)
  where rotate90 North = East
        rotate90 East = South
        rotate90 South = West
        rotate90 West = North


-- Might be an opportunity to play with lenses
move :: NavPair -> Position -> Position
move nav p = let newP = move' nav p in trace (show nav ++ "\n  -> " ++ show newP) newP
  where move' (NP North v) p = p { posY = posY p + v }
        move' (NP South v) p = p { posY = posY p - v }
        move' (NP West v) p = p { posX = posX p - v }
        move' (NP East v) p = p { posX = posX p + v }
        move' nav p = let h = heading p
	              in case newHeading nav of
                          Forward -> move' (nav {newHeading = h}) p
                          TurnLeft -> p { heading = turnLeft (value nav) h }
                          TurnRight -> p { heading = turnRight (value nav) h }

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
   -- traverse print navpairs
   print $ manhatten (foldl' (flip move) initial navpairs) initial

   putStrLn "Part b"
