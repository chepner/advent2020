import Data.Bifunctor
import Data.Functor
import Control.Arrow
import Control.Monad
import Data.List

data HiLo = Hi | Lo deriving Show
type Seat = (Int, Int)

bsp ::  Int -> Int -> [HiLo] -> Int
bsp low high xs  = go xs low high
   where go [] a _ = a
         go (Hi:rest) a b = go rest (mid a b + 1) b
         go (Lo:rest) a b = go rest a (mid a b)
         mid x y = (x + y) `div` 2


parseBoardingPass :: String -> ([HiLo], [HiLo])
parseBoardingPass = bimap (map row) (map seat) . split
   where split = take 7 &&& drop 7

row :: Char -> HiLo
row 'F' = Lo
row 'B' = Hi

seat :: Char -> HiLo
seat 'L' = Lo
seat 'R' = Hi

passToSeat :: ([HiLo], [HiLo]) -> Seat
passToSeat (row, seat) = (r, c)
   where r = bsp 0 127 row
         c = bsp 0 7 seat

seatToSeatID :: Seat -> Int
seatToSeatID (r, c) = 8*r + c

passToSeatID :: ([HiLo], [HiLo]) -> Int
passToSeatID = seatToSeatID . passToSeat

allSeats :: [Seat]
allSeats = [(r, c) | r <- [0..127], c <- [0..7]]

main = do
  contents <- readFile "day05.input"
  let boardingPasses =  map parseBoardingPass (lines contents)
      seats = sort $ map passToSeat boardingPasses
  print $ maximum (map seatToSeatID seats)
