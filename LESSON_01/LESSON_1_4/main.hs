toPart :: String -> String
toPart recipient = "Dear " ++ recipient ++ ",\n\n"

bodyPart :: String -> String
bodyPart title = "Thanks for buying \"" ++ title ++ "\".\n"

fromPart :: String -> String
fromPart author = "Thanks,\n" ++ author ++ "\n"

createEmail :: String -> String -> String -> String
createEmail recipient title author =
    toPart recipient ++ bodyPart title ++ fromPart author

main :: IO ()
main = do
    putStrLn "Who is the email for?"
    recipient <- getLine

    putStrLn "What is the Title?"
    title <- getLine

    putStrLn "Who is the Author?"
    author <- getLine

    putStrLn (createEmail recipient title author)
