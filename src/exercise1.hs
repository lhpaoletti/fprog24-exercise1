{- Exercise 1 Solutions
 -
 - Author: Luis Henrique Paoletti, 11929823
 - Date: 21.10.2024
 -}
module Exercise1 where
import qualified A1  -- qualified because of the f function
import Types


-- A2
{- Count the amount of times a number appears in a list. -}
f :: Int -> [Int] -> Int
f n l = f' n l 0

f' :: Int -> [Int] -> Int -> Int
f' _ [] acc = acc
f' n (x:xs) acc
    | n == x    = f' n xs (acc + 1)
    | otherwise = f' n xs acc


-- A3
type Position = Nat1

{- Determine in which positions of the string the character is present. -}
g :: Char -> String -> [Position]
g c s = reverse $ g' c s 1 []

g' :: Char -> String -> Nat1 -> [Position] -> [Position]
g' _ [] _ ps = ps
g' c (x:xs) i ps
    | c == x    = g' c xs (i + 1) (i:ps)
    | otherwise = g' c xs (i + 1) ps
