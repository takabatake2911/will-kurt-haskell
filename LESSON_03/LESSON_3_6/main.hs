counter :: Int -> Int
counter x = let x1 = x + 1
            in
                let x2 = x1 + 1
                    in
                        x2

main :: IO ()
main = print $ counter 1
-- output: 3