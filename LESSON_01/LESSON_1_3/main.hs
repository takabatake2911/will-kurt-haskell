f x = x + x

main :: IO ()
main = do
    print (1 + 1)
    let x = 2 + 2
    print x
    print (f 2)
