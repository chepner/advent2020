{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA

newtype Options = Options {
  fileName :: FilePath
}

options :: OA.Parser Options
options = Options <$> OA.argument OA.str (OA.metavar "FILE")

p = OA.info (options OA.<**> OA.helper)
    ( OA.fullDesc
      <> OA.progDesc "Solve Day 9 for Advent of Code 2020"
      <> OA.header "Advent 2020: Day 9" )


readNumbers :: FilePath -> IO [Int]
readNumbers fname = readFile fname >>= return . map read . lines


pairs :: [Int] -> [(Int, Int)]
pairs xs = (,) <$> xs <*> tail xs


findInvalid :: [Int] -> Int
findInvalid xs = let prefix = take 25 xs
                     target = head (drop 25 xs)
                     candidates = pairs prefix
                     match = any (\(x,y) -> x + y == target) candidates
                 in if match then findInvalid (tail xs) else target

main = do
   args <- OA.execParser p
   numbers <- readNumbers (fileName args)

   putStrLn "Part a"
   print (findInvalid numbers)
