overwrite :: Int -> Int
overwrite = \x1 -> (
    \x2 -> (
        \x3 -> (
            \x4 -> x4
            ) 4 
        ) 3 
    ) 2

main :: IO ()
main = print $ overwrite 42
-- output: 4