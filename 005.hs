-- 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
-- What is the smallest number that is evenly divisible by all of the numbers from 1 to 20?

divisibleNumbers :: [Int] -> Int -> [Int]
divisibleNumbers list divisor = filter divisible list
    where
      divisible :: Int -> Bool
      divisible element = (rem element divisor) == 0

divisibleByAll :: [Int] -> [Int] -> [Int]
divisibleByAll initialNumbers (d:divisors) = divisibleByAll newList divisors
    where
      newList :: [Int]
      newList = divisibleNumbers initialNumbers d
divisibleByAll initialNumbers [] = initialNumbers

initialList:: [Int]
initialList = [ x * 20 | x <- [1000000 .. 100000000]]

answer :: Int
answer = head (divisibleByAll initialList (reverse [1 .. 20]))

main = print answer