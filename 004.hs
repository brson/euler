-- A palindromic number reads the same both ways. The largest palindrome
-- made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

import List

main = print (head (reverse (sort allPalindromes)))

allPalindromes :: (Integral a) => [a]
allPalindromes = filter (\a -> isPalindrome (show a)) candidates

candidates :: (Integral a) => [a]
candidates = buildCandidates multipliers

buildCandidates :: (Integral a) => [a] -> [a]
buildCandidates [] = []
buildCandidates (x:xs) = (map ((*) x) xs) ++ (buildCandidates xs)

multipliers :: (Integral a) => [a]
multipliers = reverse [100 .. 999]

isPalindrome :: String -> Bool
isPalindrome phrase = phrase == (reverse phrase)