x = 4
add1 y = y + x
add2 y = (\x -> y + x) 3
add3 y = (\y -> (\x -> y + x) 1 ) 2

main :: IO ()
main = do 
    print $ add1 1
    print $ add2 1
    print $ add3 1