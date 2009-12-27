-- A Pythagorean triplet is a set of three natural numbers, a<b<c, for which,
-- a^2 + b^2 = c^2
--
-- For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2
-- There exists exactly one Pythagorean triplet for which a + b + c = 1000.
-- Find the product abc

main :: IO ()
main = print answer

answer :: Int
answer = a * b * c
    where (a, b, c) = head (filter condition triplets)
          condition (a, b, c) = a + b + c == 1000

triplets :: [(Int, Int, Int)]
triplets = [(a, b, c) | a <- range, b <- range, c <- range, a < b, b < c, a^2 + b^2 == c^2]

range :: [Int]
range = [1..1000]
