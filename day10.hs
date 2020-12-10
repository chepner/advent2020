{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Data.List (sort)
import Data.Function ((&))

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

-- |Prepend 0 for the outlet and append max+3 for the device
-- |
makeList :: [Int] -> [Int]
makeList xs = sort $ (0:xs) ++ [maximum xs + 3]

diffs :: [Int] -> [Int]
diffs xs = (zipWith (-) (tail xs) xs )

part_a :: [Int] -> Int
part_a xs = length (filter (==3) xs) * length (filter (== 1) xs)

main = do
   args <- OA.execParser p
   numbers <- readNumbers (fileName args)

   putStrLn "Part a"
   makeList numbers & diffs & part_a & print
