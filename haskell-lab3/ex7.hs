import Data.Char

onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0 = x : onlyEven xs
    | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 == 1 = x : onlyOdd xs
    | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
    | isUpper x = x : onlyUpper xs
    | otherwise = onlyUpper xs

