module Hanoi where
-- Pottern:
--
-- ab ac bc
-- ab ca cb
-- ab ac bc
-- ba ca bc
-- ab ac bc
--
--
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi n a b c =
    hanoi (n-1) a c b ++ (a, c) : hanoi (n-1) b a c
