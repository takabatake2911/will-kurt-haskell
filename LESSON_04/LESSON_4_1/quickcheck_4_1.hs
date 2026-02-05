ifEven myFunction x = if even x 
    then myFunction x 
    else x

main :: IO ()
main = do
    print $ ifEven (\x -> x^3 ) 2
    print $ ifEven (\x -> x^3 ) 3
    print $ ifEven (\x -> x^3 ) 4