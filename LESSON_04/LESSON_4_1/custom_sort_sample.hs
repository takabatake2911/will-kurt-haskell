import Data.List

author = ("Will", "Kurt")
names = [
    ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
    ]

compareLastNames :: (String, String) -> (String, String) -> Ordering
compareLastNames = \a b -> if snd a > snd b 
    then GT 
    else 
        if snd a < snd b 
            then LT 
            else EQ

main :: IO ()
main = do 
    putStrLn $ fst author
    putStrLn $ snd author
    print names
    print $ sort names
    print $ sortBy compareLastNames names