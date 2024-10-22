{- A1 Solution
 -
 - Author: Luis Henrique Paoletti, 11929823
 - Date: 21.10.2024
 -}
module A1 where
import Types


type N_value = Nat0
type K_value = Nat0


{- Mathematical function as described in A1. -}
f :: N_value -> K_value -> Nat0
f n k = div (nominator_prod k n) . product $ [1..k]

{- Nominator product function for f.
 - The product is from i=0 to (k - 1). The expression is (n - i).
 -}
nominator_prod :: K_value -> N_value -> Int
nominator_prod k n = nominator_prod' (k - 1) n 1

nominator_prod' :: Nat0 -> N_value -> Int -> Int
nominator_prod' counter n acc
    | counter < 0 = acc
    | otherwise   = nominator_prod' (counter - 1) n (acc * (n - counter))

{- Mathematical test as described in A1. -}
et :: (N_value, K_value) -> Bool
et (n, k) = et_sum n k == f (n + 1) (k + 1)

{- Sum function for the left side of et.
 - The sum is from i=0 to (n - k). The expression is (f (k + i) k).
 -}
et_sum :: N_value -> K_value -> Nat0
et_sum n k =
    let result = et_sum' (n - k) k 0
    in if result < 0
       then error "Illegal arguments"
       else result

et_sum' :: Nat0 -> K_value -> Int -> Int
et_sum' counter k acc
    | counter < 0 = acc
    | otherwise   = et_sum' (counter - 1) k (acc + f (k + counter) k)
