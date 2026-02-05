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

main :: IO ()
main = do
    print $ ifEvenInc 6
    print $ ifEvenInc 5
    print $ ifEvenDouble 6
    print $ ifEvenDouble 5
    print $ ifEvenSquare 6
    print $ ifEvenSquare 5
