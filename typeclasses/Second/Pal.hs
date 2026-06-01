module Pal where

import Data.Char

isPalindrome :: String -> Maybe Bool
isPalindrome string = isOwnReverseMaybe (rejectEmpty (normalize string))

rejectEmpty :: String -> Maybe String
rejectEmpty word =
    case word of
        [] -> Nothing
        _ -> Just word

normalize :: String -> String
normalize string =
    filter notPunctuation (filter notSpace (allLowerCase string))

isOwnReverse :: String -> Bool
isOwnReverse word =
    word == reverse word

isOwnReverseMaybe :: Maybe String -> Maybe Bool
isOwnReverseMaybe maybeString =
    case maybeString of
        Nothing -> Nothing                       
        Just string -> Just (isOwnReverse string)

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