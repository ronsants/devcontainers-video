import Pal

-- The interactive program --

main :: IO ()
main =
  do
    word <- getLine
    print (verbose word)

verbose :: String -> String
verbose word =
  case isPalindrome word of
    Nothing -> "Please enter a word."
    Just False -> "Sorry, this word is not a palindrome."
    Just True -> "Congratulations, this word is a palindrome!"