-- Each new term in the Fibonacci sequence is generated
-- by adding the previous two terms. By starting with 1
-- and 2, the first 10 terms will be:
--
-- 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
--
-- Find the sum of all the even-valued terms in the sequence
-- which do not exceed four million.

type Criteria a = a -> Bool

main = print (sumWithCriteria even max fiblist)
    where max = 4000000

          sumWithCriteria :: (Ord a, Num a) => Criteria a -> a -> [a] -> a
          sumWithCriteria _ 0 _ = 0
          sumWithCriteria criteria max (x:xs)
              | max < x = 0
              | (criteria x) = x + (sumWithCriteria criteria (max - 1) xs)
              | otherwise = sumWithCriteria criteria (max - 1) xs

          fiblist = 0 : 1 : (zipWith (+) fiblist (tail fiblist))



