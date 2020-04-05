isPalindrome :: [Char] -> Bool
isPalindrome s = if (null s || length s == 1) then True
                 else if (head s /= last s) then False
                 else isPalindrome (init (tail s))

toUpper :: Char -> Char
toUpper x = toEnum (fromEnum x - 32)
                
toLower :: Char -> Char
toLower x = toEnum (fromEnum x + 32)

capitalize :: [Char] -> [Char]
capitalize w = 
    toUpper (head w) : tail w