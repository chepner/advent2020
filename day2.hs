module Day2 where

import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

data Entry = Entry Int Int Char String deriving Show

number :: ReadP Int
number = read <$> many1 (satisfy isDigit)

letter :: ReadP Char
letter = satisfy isLower

range :: ReadP (Int, Int)
range = do
  n1 <- number
  char '-'
  n2 <- number
  return (n1, n2)

entry :: ReadP Entry
entry = do
  (low, high) <- range
  skipSpaces
  char <- letter
  string ": "
  password <- many1 letter
  eof
  return $ Entry low high char password


parseEntries :: String -> [Entry]
parseEntries s = lines s >>= readP_to_S entry >>= return . fst

verifyA :: Entry -> Bool
verifyA (Entry low high char password) = low <= n && n <= high
   where n = length (filter (== char) password)

verifyB :: Entry -> Bool
verifyB (Entry low high char password) = (c1 == char || c2 == char ) && c1 /= c2
   where c1 = password !! (low - 1)
         c2 = password !! (high - 1)

countValid :: (Entry -> Bool) -> [Entry] -> Int
countValid = (length . ) . filter

main = do
    entries <- parseEntries <$> readFile "day2.input"

    -- part a
    print $ countValid verifyA entries

    -- part b
    print $ countValid verifyB entries
