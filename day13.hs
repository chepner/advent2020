{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Data.List
import Data.List.Split
import Data.Char
import Data.Either
import Debug.Trace
import System.IO

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

data BusNotes = BusNotes { timestamp :: Int, busIDs :: [Either String Int] } deriving Show

readBusNotes :: FilePath -> IO BusNotes
readBusNotes fname = withFile fname ReadMode $ \fh -> do
  ts <- read <$> hGetLine fh
  idLine <- hGetLine fh
  let ids =  splitOn "," idLine
  return $ BusNotes ts (map (\x -> if all isDigit x then Right (read x) else Left x) ids)
  
will_depart :: Either String Int -> Int -> Bool
will_depart (Left _) _ = True
will_depart (Right b) ts = mod ts b == 0
  
part_b :: [Either String Int] -> Int -> Bool
part_b departures t = let pairs = zip departures [t..]
                      in all (uncurry will_depart) pairs

-- End command-line boilerplate
main = do
   args <- OA.execParser p
   busnotes  <- readBusNotes (fileName args)

   putStrLn "Part a"
   let ts = timestamp busnotes
       ids = rights $ busIDs busnotes
   let (offset, bus) = maximum ( map (\x -> (mod ts x - x, x)) ids)
       wait = abs offset
   print wait
   print bus
   print $ wait * bus


   -- Possibly correct, but will almost certainly be too slow.
   -- Need a better search strategy
   putStrLn "Part b"
   let busids = busIDs busnotes
       firstBus = head (rights busids)
   
   print firstBus
   print $ True `elemIndex` (map (part_b (busIDs busnotes)) [0,firstBus..])

