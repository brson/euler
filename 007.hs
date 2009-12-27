-- Find the 10001st prime

main :: IO ()
main = print answer

answer :: Int
answer = primes !! nthPrimeIdx

nthPrimeIdx :: Int
nthPrimeIdx = nthPrime - 1

nthPrime :: Int
nthPrime = 10001

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
