main :: IO ()
main = do
    let f = \x -> x * 2
    print $ f 4
    print $ f 5
    print $ f 6
    