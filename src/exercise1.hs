{- Exercise 1 Solutions
 -
 - Author: Luis Henrique Paoletti, 11929823
 - Date: 21.10.2024
 -}
module Exercise1 where
import qualified A1  -- qualified because of the f function
import Types

import Data.List


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

{- Determine in which positions of the string the character is present.
 - The position 1 is at index 0, and so on.
 -}
g :: Char -> String -> [Position]
g c s = reverse $ g' c s 1 []

g' :: Char -> String -> Nat1 -> [Position] -> [Position]
g' _ [] _ ps = ps
g' c (x:xs) i ps
    | c == x    = g' c xs (i + 1) (i:ps)
    | otherwise = g' c xs (i + 1) ps


-- A4
{- Determine which numbers in a list are lesser and which are greater than a given number from the list.
 - The resulting lists do not contain duplicates and are both in ascending order.
 - If there's neither a number that's lesser nor greater, both lists are given as empty lists.
 - If the number is not in the list at all, an error is thrown.
 -}
h :: Integer -> [Integer] -> ([Integer], [Integer])
h i xs = if i `elem` xs
         then (lesser i xs, greater i xs)
         else error "Number must be an element of the given list"

lesser :: Integer -> [Integer] -> [Integer]
lesser i xs  = filter (< i) . remDup $ xs

greater :: Integer -> [Integer] -> [Integer]
greater i xs = filter (> i) . remDup $ xs

{- Remove duplicates from the given list.
 - The resulting list is in ascending order.
 -}
remDup :: [Integer] -> [Integer]
remDup xs = sort . remDup' [] . sort $ xs

{- The given list of numbers must be sorted in ascending order.
 - The resulting list is in descending order.
 -}
remDup' :: [Integer] -> [Integer] -> [Integer]
remDup' acc []    = acc
remDup' [] (x:xs) = remDup' [x] xs  -- init acc
remDup' acc@(y:_) (x:xs)
    | y == x      = remDup' acc xs
    | otherwise   = remDup' (x:acc) xs  -- y < x
