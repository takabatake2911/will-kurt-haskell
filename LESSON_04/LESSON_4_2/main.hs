import Data.List

addressLetter :: (String, String) -> String -> String 
addressLetter name location =
    nameText ++ " - " ++ location
  where
    nameText = fst name ++ " " ++ snd name

main :: IO ()
main = do 
    print $ addressLetter ("Bob", "Smith") "P0 Box 1234 - San Francisco, CA, 94111"
