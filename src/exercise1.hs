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
f n = length . filter (== n)


-- A3
type Position = Nat1

{- Determine in which positions of the string the character is present.
 - The position 1 is at index 0, and so on.
 -}
g :: Char -> String -> [Position]
g c s = [i | (i, c') <- zip [1..] s, c == c']


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

{- Remove duplicates from the given list. -}
remDup :: [Integer] -> [Integer]
remDup []     = []
remDup (x:xs) = x:(remDup . filter (/= x) $ xs)
