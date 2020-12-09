{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import qualified Options.Applicative as OA
import Data.List (inits)

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

-- Should use dynamic programming for part b, but this is fast enough
generateRanges :: [Int] -> [[Int]]
-- ^ Generate all subranges (with at least 2 elements) of a list of integers. For example,
-- generateRange [1,2,3] = [[1,2],[1,2,3],[2,3]]
-- As currently written, this would not work on infinite lists.
generateRanges [] = []
generateRanges xs = (drop 2 (inits xs)) ++ generateRanges (tail xs)

-- | Find a range that sums to a target value
findRange :: [Int] -> Int -> [Int]
findRange xs t = head $ filter ((==) t . sum) (generateRanges xs)

main = do
   args <- OA.execParser p
   numbers <- readNumbers (fileName args)

   putStrLn "Part a"
   let part_a = findInvalid numbers
   print part_a

   putStrLn "Part b"
   let result = findRange numbers part_a
   print $ minimum result + maximum result
