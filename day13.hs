{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Data.List
import Data.List.Split
import Data.Char
import Data.Either
import System.IO
import Math.NumberTheory.Euclidean

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

data BusNotes = BusNotes { timestamp :: Int
                         , busIDs :: [Either String Int]
                         } deriving Show


readBusNotes :: FilePath -> IO BusNotes
readBusNotes fname = withFile fname ReadMode $ \fh -> do
  ts <- read <$> hGetLine fh
  ids <- splitOn "," <$> hGetLine fh
  let simpleParse x = if all isDigit x
                      then Right (read x)
                      else Left x
  return $ BusNotes ts (map simpleParse ids)
  

main = do
   args <- OA.execParser p
   busnotes <- readBusNotes (fileName args)

   putStrLn "Part a"
   let ts = timestamp busnotes
       ids = rights $ busIDs busnotes
   let (offset, bus) = maximum ( map (\x -> (mod ts x - x, x)) ids)
       wait = abs offset
   print $ wait * bus

   putStrLn "Part b"
   -- The bus IDs are the moduli in a residue number system. Their position in
   -- the list (when subtracted from the bus ID) the residue for the bus ID.

   let busids = busIDs busnotes
       pairs_to_check = [ (i, b) | (i, Right b) <- zip [(0::Int)..] busids]

       (offsets, nis) = unzip pairs_to_check
       ais = map negate offsets
       n = product nis
       bignis = map (div n) nis
       bigmis = [let (_, x, _) = extendedGCD a b in x | (a, b) <- zip bignis nis]

   print $ sum [x*y*z | (x, y, z) <- zip3 ais bigmis bignis] `mod` n
