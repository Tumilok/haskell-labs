absInt n =
    case (n >= 0) of
      True -> n
      _    -> -n

isItTheAnswer :: String -> Bool
isItTheAnswer s =
    case s of
        "Love" -> True
        _      -> False

not' :: Bool -> Bool
not' x =
    case x of
        True -> False
        False -> True

or' :: (Bool, Bool) -> Bool
or' (a, b) = 
    case a of
        True -> True
        False -> case b of
            True -> True
            False -> False

and' :: (Bool, Bool) -> Bool
and' (a, b) case a of
    False -> False
    True -> case b of
        True -> True
        False -> False
        
--nand' :: (Bool, Bool) -> Bool
--xor' :: (Bool, Bool) -> Bool