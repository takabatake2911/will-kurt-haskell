overwrite :: Int -> Int
overwrite x = let x = 2
                in
                    let x = 3
                    in
                        let x = 4
                        in
                            x

main :: IO ()
main = print $ overwrite 42
-- output: 4