{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Text.ParserCombinators.ReadP
import Data.List (foldl')
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

data Direction = North | South | East | West deriving Show
data Action = Go Direction | Advance | TurnLeft | TurnRight deriving Show

data NavPair = NP { action :: Action, value :: Int } deriving Show

navpair :: ReadP NavPair
navpair = do
    d <- satisfy (`elem` "NSEWFLR")
    n <- many1(satisfy isDigit)
    eof
    return $ NP (case d of
               'N' -> Go North
               'S' -> Go South
               'E' -> Go East
               'W' -> Go West
               'F' -> Advance
               'L' -> TurnLeft
               'R' -> TurnRight) (read n)

data Position = P { posX, posY :: Int } deriving Show
data FerryState = FS { heading :: Direction, position :: Position } deriving Show

initial :: FerryState
initial = FS East (P 0 0)


turnLeft, turnRight :: Int -> Direction -> Direction
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

translate :: Position -> Direction -> Int -> Position
translate (P x y) North v = (P x (y + v))  -- P x . (y +)
translate (P x y) South v = (P x (y - v))  -- P x . (y -)
translate (P x y) East v = (P (x + v) y)   -- flip P y . (x +)
translate (P x y) West v = (P (x - v) y)   -- flip P y . (x -)


-- Might be an opportunity to play with lenses
move :: NavPair -> FerryState -> FerryState
move (NP (Go d) v) (FS h p) = FS h (translate p d v)
move (NP Advance v) (FS h p) = FS h (translate p h v)
move (NP TurnLeft v) (FS h p) = FS (turnLeft v h) p
move (NP TurnRight v) (FS h p) = FS (turnRight v h) p

manhatten :: Position -> Position -> Int
manhatten (P x2 y2) (P x1 y1) = abs (x2 - x1) + abs (y2 - y1)

-- readNavPairs :: FilePath -> IO [NavPair]
readNavPairs fname = do
    contents <- lines <$> readFile fname
    return $ map (fst . (!! 0) . readP_to_S navpair) contents 


main = do
   args <- OA.execParser p
   navpairs <- readNavPairs (fileName args)

   putStrLn "Part a"
   -- traverse print navpairs
   let final = foldl' (flip move) initial navpairs
   print $ manhatten (position final) (position initial)

   putStrLn "Part b"
