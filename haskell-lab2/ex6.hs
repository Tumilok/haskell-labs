fib :: (Num a, Eq a) => a -> a
fib n = 
    if n == 0 || n == 1 then n
    else fib (n - 2) + fib (n - 1)

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' []    = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' []  = 0
length' (x:xs) = 1 + length' xs

or' :: [Bool] -> Bool
or' []  = False
or' (x:xs) = 
    if x == True then True
    else or' xs

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) =
    if x == False then False
    else and' xs


sum'2 :: Num a => [a] -> a
sum'2 xs = loop 0 xs
    where loop acc []       = acc
          loop acc (x:xs)   = loop (x + acc) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where loop acc []       = acc
          loop acc (x:xs)   = loop (x + acc) xs

iloczyn :: Num a => [a] -> a
iloczyn = loop 0
    where loop acc []       = 1
          loop acc (x:xs)   = loop (x * acc) xs