import Data.Char

-- Data.Char - isUpper, isLower, isLetter, toUpper, toLower

countLetters :: String -> Int
countLetters xs = length [x | x <- xs, isLetter x]

strToUpper :: String -> String
strToUpper xs = [toUpper x | x <- xs]
