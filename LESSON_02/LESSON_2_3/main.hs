calcChange owed given = if change > 0
        then change 
        else 0
    where
        change = given - owed

f x = x ^ 2

x = 2

-- x = 3  error: Multiple declarations of ‘x’

main :: IO ()
main = do 
    print x
    print $ calcChange 2 5
    print $ calcChange 5 2
    print $ f 8