ifEvenInc :: Int -> Int 
ifEvenInc n = if even n 
    then n + 1 
    else n

ifEvenDouble :: Int -> Int 
ifEvenDouble n = if even n 
    then n * 2  
    else n

ifEvenSquare :: Int -> Int 
ifEvenSquare n = if even n 
    then n^2  
    else n

ifEven :: (Int -> Int) -> Int -> Int
ifEven myFunction x = if even x 
    then myFunction x 
    else x

inc n = n + 1
double n = n * 2
square n = n^2

main :: IO ()
main = do
    print $ ifEvenInc 6
    print $ ifEvenInc 5
    print $ ifEvenDouble 6
    print $ ifEvenDouble 5
    print $ ifEvenSquare 6
    print $ ifEvenSquare 5
    print $ ifEven inc 6
    print $ ifEven inc 5
    print $ ifEven double 6
    print $ ifEven double 5
    print $ ifEven square 6
    print $ ifEven square 5
