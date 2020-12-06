import System.Environment
import Data.List ( nub -- I know, it's quadratic...
                 , intersect
                 )
                  

-- Assumes well-behaved input
groupSplit :: [String] -> [[String]]
groupSplit [] = [[]]
groupSplit ("":rest) = [] : groupSplit rest
groupSplit (x:rest) = let (y:result) = groupSplit rest
                      in (x:y) : result

main = do
  fname <- fmap (!! 0) getArgs
  contents <- readFile fname
  let groups  = groupSplit (lines contents)

  putStrLn "Part a"
  print $ sum $ map (length . nub. concat) groups

  putStrLn "Part b"
  print $ sum $ map (length . (foldr1 intersect)) groups
