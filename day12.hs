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
data FerryState t = FS { heading :: t, position :: Position } deriving Show

initial_a :: FerryState Direction
initial_a = FS East (P 0 0)

initial_b :: FerryState Position
initial_b = FS (P 10 1) (P 0 0)


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

rotateLeft, rotateRight :: Position -> Int -> Position
rotateLeft p 0 = p
rotateLeft p 360 = rotateLeft p 0
rotateLeft (P x y) 90 = P (negate y) x  -- (x + iy)*i == -y + ix
rotateLeft (P x y) 180 = P (negate x) (negate y)  -- (x + iy)*i**2 == -x - iy
rotateLeft (P x y) 270 = P y (negate x)  -- (x + iy)*(-i) == y - ix

rotateRight p v = rotateLeft p (360 - v)


-- Might be an opportunity to play with lenses
move_a :: NavPair -> FerryState Direction -> FerryState Direction
move_a (NP (Go d) v) (FS h p) = FS h (translate p d v)
move_a (NP Advance v) (FS h p) = FS h (translate p h v)
move_a (NP TurnLeft v) (FS h p) = FS (turnLeft v h) p
move_a (NP TurnRight v) (FS h p) = FS (turnRight v h) p

move_b :: NavPair -> FerryState Position -> FerryState Position
move_b (NP (Go d) v) fs = fs { heading = translate (heading fs) d v }
move_b (NP Advance v) fs = let wp = heading fs
                               p = position fs
                               (x, y) = (posX p, posY p)
                               (dx, dy) = (posX wp, posY wp)
                           in fs {position = p {posX = x + v*dx,
                                                posY = y + v*dy}}
move_b (NP TurnLeft v) (FS wp p) = FS (rotateLeft wp v) p
move_b (NP TurnRight v) (FS wp p) = FS (rotateRight wp v) p


manhatten :: Position -> Position -> Int
manhatten (P x2 y2) (P x1 y1) = abs (x2 - x1) + abs (y2 - y1)

-- readNavPairs :: FilePath -> IO [NavPair]
readNavPairs fname = do
    contents <- lines <$> readFile fname
    return $ map (fst . (!! 0) . readP_to_S navpair) contents 


-- Used to trace the results of the foldl'
debug f np fs = let result = f np fs in trace (show np ++ "\n ++ " ++ show result) result

main = do
   args <- OA.execParser p
   navpairs <- readNavPairs (fileName args)

   putStrLn "Part a"
   -- traverse print navpairs
   let final_a = foldl' (flip move_a) initial_a navpairs
   print $ manhatten (position final_a) (position initial_a)

   putStrLn "Part b"
   let final_b = foldl' (flip $ debug move_b) initial_b navpairs
   print $ manhatten (position final_b) (position initial_b)
