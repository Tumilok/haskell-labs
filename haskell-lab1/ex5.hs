sgn :: Int -> Int
sgn n = if n < 0
    then -1
    else if n == 0
        then 0
        else 1

absInt' :: Int -> Int
absInt x = if x < 0
    then x * (-1)
    else x

min2Int :: (Int, Int) -> Int
min2Int (x, y) = if x < y
    then x
    else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x, y, z) = min2Int(min2Int(x, y), z)

toUpper :: Char -> Char
toUpper x = toEnum (fromEnum x - 32)

toLower :: Char -> Char
toLower x = toEnum (fromEnum x + 32)

isDigit :: Char -> Bool
isDigit x = if y > 47 && y < 58
    then True
    else False
    where y = fromEnum x

charToNum :: Char -> Int
charToNum x = if isDigit x == False then -1
else
    if fromEnum x == 48 then 0 
    else if fromEnum x == 49 then 1
    else if fromEnum x == 50 then 2
    else if fromEnum x == 51 then 3
    else if fromEnum x == 52 then 4
    else if fromEnum x == 53 then 5
    else if fromEnum x == 54 then 6
    else if fromEnum x == 55 then 7
    else if fromEnum x == 56 then 8
    else 9
