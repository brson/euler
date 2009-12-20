-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

main = do print (largestPrimeFactor 600851475143)

largestPrimeFactor :: (Integral a) => a -> a
largestPrimeFactor number
    | (smallestPrimeFactor number) == number = number
    | otherwise = largestPrimeFactor (newTarget number)

    where newTarget :: (Integral a) => a -> a
          newTarget number = number `div` (smallestPrimeFactor number)

          smallestPrimeFactor :: (Integral a) => a -> a
          smallestPrimeFactor number = head (filter (\n -> isPrimeFactorOf n number) [2 .. number])

isPrimeFactorOf :: (Integral a) => a -> a -> Bool
isPrimeFactorOf factor number = (isFactorOf factor number) && (isPrime factor)

isFactorOf :: (Integral a) => a -> a -> Bool
isFactorOf factor number = (number `mod` factor) == 0

isPrime :: (Integral a) => a -> Bool
isPrime num = null (composites num)
    where composites num = filter (isAMultipleOf num) [2 .. num - 1]
          isAMultipleOf num factor = (num `mod` factor) == 0

