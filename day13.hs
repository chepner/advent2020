{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Data.List.Split
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

data BusNotes = BusNotes { timestamp :: Int, busIDs :: [Int] } deriving Show

readBusNotes :: FilePath -> IO BusNotes
readBusNotes fname = withFile fname ReadMode $ \fh -> do
  ts <- read <$> hGetLine fh
  idLine <- hGetLine fh
  let ids = filter (/= "x") $ splitOn "," idLine
  return $ BusNotes ts (map read ids)
  
  

-- End command-line boilerplate
main = do
   args <- OA.execParser p
   busnotes <- readBusNotes (fileName args)

   putStrLn "Part a"
   print busnotes
