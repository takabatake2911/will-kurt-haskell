-- 'a' is a polymorphic (generic) type variable
simple :: a -> a
simple x = x

main :: IO ()
main = do
    print $ simple 2
    print $ simple "dog"