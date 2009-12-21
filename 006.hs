sumOfSquares :: [Float] -> Float
sumOfSquares xs = sum squares
    where
      squares :: [Float]
      squares = map square xs

squareOfSum :: [Float] -> Float
squareOfSum xs = square (sum xs)

square :: Float -> Float
square x = x ** 2.0

difference :: [Float] -> Float
difference xs = (squareOfSum xs) - (sumOfSquares xs)

answer :: Int
answer = truncate (difference [1..100])

main :: IO ()
main = print answer
