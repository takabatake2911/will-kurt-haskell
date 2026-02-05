-- A lambda that chooses the larger of two numbers
body :: Int -> Int -> Int
body = \sumSquare squareSum ->
    if sumSquare > squareSum
        then sumSquare
        else squareSum

-- f computes sumSquare and squareSum, then applies body
f :: Int -> Int -> Int
f x y =
    let sumSquare = x^2 + y^2
        squareSum = (x + y)^2
    in body sumSquare squareSum

main :: IO ()
main = print $ f 3 5
