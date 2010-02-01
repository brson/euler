import Maybe
import Data.List

triangleNumbers :: [Int]
triangleNumbers = [sum [1..x] | x <- [1..]]

factors :: Int -> [Int]
factors x = filter ((==) 0 . mod x) [1..limit] ++ [x]
    where
      limit :: Int
      limit = x `div` 2

triangleFactors :: [[Int]]
triangleFactors = map factors triangleNumbers

trianglePairs :: [(Int, Int)]
trianglePairs = zip triangleNumbers triangleFactorCounts

triangleFactorCounts :: [Int]
triangleFactorCounts = map length triangleFactors

targetFactors :: Int
targetFactors = 500

answerPair :: Int -> (Int, Int)
answerPair targetFactors = fromJust (find (\ (_, factorCount) -> factorCount > targetFactors) trianglePairs)

answer :: Int
answer = answerNumber
    where (answerNumber, _) = answerPair targetFactors

main :: IO ()
main = print answer