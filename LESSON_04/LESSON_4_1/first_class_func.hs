ifEven :: (Int -> Int) -> Int -> Int
ifEven myFunction x = if even x 
    then myFunction x 
    else x

inc n = n + 1
double n = n * 2
square n = n^2

main :: IO ()
main = do
    print $ ifEven inc 6
    print $ ifEven inc 5
    print $ ifEven double 6
    print $ ifEven double 5
    print $ ifEven square 6
    print $ ifEven square 5
