import Data.Char

-- The interactive program --

main :: IO ()
main =
  do
    word <- getLine
    print (verbose word)

verbose :: String -> String
verbose word =
    case (nonemptyPal word) of
        Nothing -> "Please enter a word."
        Just False -> "Sorry, this word is not a palindrome."
        Just True -> "Congratulations, this word is a palindrome!"

-- What a palindrome is --

isOwnReverse :: String -> Bool
isOwnReverse word =
    word == reverse word

nonemptyPal :: String -> Maybe Bool
nonemptyPal word =
    case word of
        [] -> Nothing
        _ -> Just (isOwnReverse word)

allLowerCase :: String -> String
allLowerCase word = map toLower word

isPalindromeIgnoringCase :: String -> Bool
isPalindromeIgnoringCase word = isOwnReverse (allLowerCase word)

isPalindromePhrase :: String -> Bool
isPalindromePhrase phrase =
    isOwnReverse (filter notSpace phrase)

notSpace :: Char -> Bool
notSpace x = not (x == ' ')

notPunctuation :: Char -> Bool
notPunctuation x = not (isPunctuation x)