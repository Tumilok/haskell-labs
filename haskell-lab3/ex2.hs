sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumSqr' :: Num a => [a] -> a
sumSqr' (x:xs) = x^2 + sumSqr' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith _ [] = 0
sumWith f (x:xs) = f x + sumWith f xs

sum, sumSqr, sumCub, sumAbs :: [Integer] -> Integer

sum = sumWith(\e -> e)
sumSqr = sumWith(\e -> e ^ 2)
sumCub = sumWith(\e -> e ^ 3)
sumAbs = sumWith(\e -> abs e)