f n = if even n then n - 2 else 3 * n + 1

main :: IO()
main = do
    print $ f 1
    print $ f 2
    print $ f 3
    print $ f 4
    print $ f 5