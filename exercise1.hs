{- Exercise 1 Solutions
 -
 - Author: Luis Henrique Paoletti, 11929823
 - Date: 21.10.2024
 -}

type Nat0 = Int
type N_value = Nat0
type K_value = Nat0


{- A1 function. -}
f :: N_value -> K_value -> Nat0
f n k = (nominator_sum k n) `div` (denominator_sum k)

{- Nominator Sum function for A1 function.
 - The sum is from i=0 to (k - 1). The expression is (n - i).
 -}
nominator_sum :: K_value -> N_value -> Nat0
nominator_sum k n = nominator_sum' (k - 1) n 0

nominator_sum' :: Nat0 -> N_value -> Nat0 -> Nat0
nominator_sum' counter n acc
    | counter <= -1 = acc
    | otherwise     = nominator_sum' (counter - 1) n (acc + n - counter)

{- Denominator Sum function for A1 function.
 - The sum is from i=1 to k. The expression is (i);
 -}
denominator_sum :: K_value -> Nat0
denominator_sum k = denominator_sum' k 0

denominator_sum' :: Nat0 -> Nat0 -> Nat0
denominator_sum' counter acc
    | counter <= 0 = acc
    | otherwise    = denominator_sum' (counter - 1) (acc + counter)
