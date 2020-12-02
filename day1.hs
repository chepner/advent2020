module Day1 where

import Data.List

parseEntries :: String -> [Integer]
parseEntries = map read . lines

makePairs :: [Integer] -> [(Integer, Integer)]
makePairs xs = (,) <$> xs <*> xs

makeTriples :: [Integer] -> [(Integer, Integer, Integer)]
makeTriples xs = (,,) <$> xs <*> xs <*> xs

findPair :: [(Integer, Integer)] -> Maybe (Integer, Integer)
findPair = find (\(x,y) -> x + y == 2020)

findTriple :: [(Integer, Integer, Integer)] -> Maybe (Integer, Integer, Integer)
findTriple = find (\(x,y,z) -> x + y + z == 2020)
  

main = do
  
    lines <- parseEntries <$> readFile "day1a.input"
    let (Just (x, y)) = findPair (makePairs lines)
    print $ x * y
    let (Just (x, y,z)) = findTriple (makeTriples lines)
    print $ x * y * z
  
