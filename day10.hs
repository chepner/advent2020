{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Data.List (sort, tails)
import Data.Function ((&))
import Control.Monad (guard)

data Options = Options {
    fileName :: FilePath
  , count :: Int
}

options :: OA.Parser Options
options = Options
          <$> OA.argument OA.str (OA.metavar "FILE")
          <*> OA.option OA.auto (OA.long "count" <> OA.value 20)

p = OA.info (options OA.<**> OA.helper)
    ( OA.fullDesc
      <> OA.progDesc "Solve Day 10 for Advent of Code 2020"
      <> OA.header "Advent 2020: Day 10" )


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


arrangements :: [Int] -> [[Int]]
arrangements [x] = [[x]]
arrangements (x:xs) = let p [] = False
                          p (y:_) = y <= x + 3
                          continuations = filter p (tails xs)
                      in map (x:) (continuations >>= arrangements)

countArrangements :: [Int] -> Int
countArrangements [x] = 1
countArrangements (x:xs) = let p [] = False
                               p (y:_) = y <= x + 3
                               continuations = filter p (tails xs)
                           in sum (map countArrangements continuations)

main = do
   args <- OA.execParser p
   numbers <- readNumbers (fileName args)

   putStrLn "Part a"
   makeList numbers & diffs & part_a & print

   putStrLn "Part b"
   print (makeList numbers)
   makeList numbers & take (count args) & countArrangements & print
