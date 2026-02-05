import Data.List

names = [
    ("Ian", "Curtis"),
    ("Bernard", "Sumner"),
    ("Peter", "Hook"),
    ("Stephen", "Morris")
    ]

compareLastNames a b = if snd a > snd b 
    then GT 
    else if snd a < snd b 
        then LT 
        else if fst a > fst b 
            then GT   
            else if fst a < fst b 
                then LT 
                else EQ

main :: IO ()
main = do
    print $ sort names 
    print $ sortBy compareLastNames names 
