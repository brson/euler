main :: IO ()
main = print answer

answer :: Integer
answer = sum primesUnder2Mil

maxPrime :: Integer
maxPrime = 2000000

primesUnder2Mil :: [Integer]
primesUnder2Mil = takeWhile (< maxPrime) primes'

primes :: [Integer]
primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

primes' :: [Integer]
primes' = 2: 3: sieve (tail primes) [5,7..]
    where 
      sieve (p:ps) xs = h ++ sieve ps [x | x<-t, x `rem` p /= 0]  
          -- or:  filter ((/=0).(`rem`p)) t
          where (h,~(_:t)) = span (< p*p) xs