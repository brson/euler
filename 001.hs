-- If we list all the natural numbers below 10 that are multiples of 3
-- or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
-- Find the sum of all the multiples of 3 or 5 below 1000.


main :: IO ()
main = do print (findSumOfMultiples factors range)
    where factors = [3, 5]
          range = [3 .. 999]

findSumOfMultiples factors range
    = sum (filter (pickMultiples factors) range)

pickMultiples :: [Integer] -> Integer -> Bool
pickMultiples [] _ = False
pickMultiples (factor:factors) num 
    = (isMultiple factor num) || (pickMultiples factors num)

isMultiple :: Integer -> Integer -> Bool
isMultiple factor num = (num `mod` factor) == 0

       
