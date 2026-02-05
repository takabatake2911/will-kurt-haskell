f :: Int -> Int -> Int
f x y =
    if sumSquare > squareSum
        then sumSquare
        else squareSum
  where
    sumSquare = x^2 + y^2
    squareSum = (x + y)^2

main :: IO ()
main = print $ f 2 3
