main :: IO ()
main = do
    print $ (\x -> x) 4
    print $ (\x -> x) "hi"
    print $ (\x -> x) [1,2,3]