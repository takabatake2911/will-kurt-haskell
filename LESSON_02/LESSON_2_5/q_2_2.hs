inc :: Int -> Int
inc x = x + 1

double :: Int -> Int
double x = x * 2 

square :: Int -> Int
square x = x ^ 2 

main :: IO ()
main = do
    print $ inc 5 
    print $ double 5
    print $ square 5