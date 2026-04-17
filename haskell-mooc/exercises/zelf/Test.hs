module Test where
repeatString n str = repeatHelper n str ""

repeatHelper n str result = if (n==0)
                            then result
                            else repeatHelper (n-1) str (result++str)

repeatString' n str = repeatHelper' n str ""

repeatHelper' 0 _   result = result
repeatHelper' n str result = repeatHelper' (n-1) str (result++str)   


-- fibonacci numbers, fast version
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)

factorial n
  | n<0       = -1
  | n==0      = 1
  | otherwise = n * factorial (n-1)

guessAge :: String -> Int -> String
guessAge "Griselda" age
    | age < 47 = "Too low!"
    | age > 47 = "Too high!"
    | otherwise = "Correct!"
guessAge "Hansel" age
    | age < 12 = "Too low!"
    | age > 12 = "Too high!"
    | otherwise = "Correct!"
guessAge name age = "Wrong name!"  