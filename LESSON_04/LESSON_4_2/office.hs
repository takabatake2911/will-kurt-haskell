sfOffice name =
    if lastName < "L"
        then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
        else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
  where 
    firstName = fst name
    lastName  = snd name
    nameText  = firstName ++ " " ++ lastName

nyOffice name =
    nameText ++ ": PO Box 789 - New York, NY, 10013"
  where
    nameText = fst name ++ " " ++ snd name

renoOffice name =
    nameText ++ " - PO Box 456 - Reno, NV 89523"
  where
    nameText = snd name

getLocationFunction location =
    case location of 
        "ny"   -> nyOffice
        "sf"   -> sfOffice
        "reno" -> renoOffice
        _      -> (\name -> fst name ++ " " ++ snd name)

main :: IO ()
main = do
    let name = ("Bob", "Smith")
    print $ getLocationFunction "sf" name
    print $ getLocationFunction "ny" name
    print $ getLocationFunction "reno" name
    print $ getLocationFunction "other" name
