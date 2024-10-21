{- Exercise 1 Solutions
 -
 - Author: Luis Henrique Paoletti, 11929823
 - Date: 21.10.2024
 -}
module Exercise1 where
import qualified A1  -- qualified because of the f function


-- A2
{- Count the amount of times a number appears in a list. -}
f :: Int -> [Int] -> Int
f n l = f' n l 0

f' :: Int -> [Int] -> Int -> Int
f' _ [] acc = acc
f' n (x:xs) acc
    | n == x    = f' n xs (acc + 1)
    | otherwise = f' n xs acc
