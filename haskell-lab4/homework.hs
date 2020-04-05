data Person = Person String String Integer deriving (Show)

firstname :: Person -> String
firstname (Person n _ _) = n

lastname :: Person -> String
lastname (Person _ sn _) = sn

age :: Person -> Integer
age (Person _ _ a) = a

data Human = Human
            { fname :: String 
            , surname :: String
            } deriving (Show)


data Car a b c = Car 
            { company :: a
            , model :: b
            , year :: c
            } deriving (Show)

carInfo :: (Show a) => Car String String a -> String
carInfo (Car {company = c, model = m, year = y}) = 
    "This " ++ c ++ " " ++ m ++ " was made in " ++ show y


-- Algebraiczne typy danych (BSTree)

data Tree a = Nil |
              Node a (Tree a) (Tree a)
              deriving (Eq, Ord, Show, Read)

depth :: Tree a -> Int
depth Nil = 0
depth (Node n lt rt) = 1 + max (depth lt) (depth rt)

collapse :: Tree a -> [a]
collapse Nil = []
collapse (Node n lt rt) = collapse lt ++ [n] ++ collapse rt

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Nil = Nil
mapTree f (Node n lt rt) = Node (f n) (mapTree f lt) (mapTree f rt)


-- Typy wyższego rzędu (kinds)

data CDT a b =
    CDT { e :: Either a b
        , f :: Either a b -> Maybe a }


-- 2. Klasy tyþów w Haskellu

-- Klasy typów i ich instancje:

newtype Vec2Dnt a = Vec2Dnt (a, a)
instance Eq a => Eq (Vec2Dnt a) where
    (==) (Vec2Dnt (x1,y1)) (Vec2Dnt (x2,y2)) = x1 == x2 && y1 == y2


data Foo a = Foo 
            { value :: a
            , name :: String
            }

instance Show a => Show (Foo a) where
      show Foo{ value = v, name = n } = "Name: " ++ n ++ " with " ++ show v


data MyType = C1 Int | C2 Double Bool
instance Eq MyType where
    (==) (C1 n) (C2 a b) = False
    (==) (C2 a b) (C1 n) = False
    (==) (C1 n1) (C1 n2) = n1 == n2
    (==) (C2 a1 b1) (C2 a2 b2) = a1 == a2 && b1 == b2


data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

minElemOf :: Ord a => BinTree a -> a
minElemOf EmptyBT = 100
minElemOf (NodeBT n lt rt) = min (min n (minElemOf lt)) (minElemOf rt)


data BST a = EmptyBST | NodeBST (BST a) a (BST a)

insert2BST :: Ord a => a -> BST a -> BST a
insert2BST x EmptyBST = NodeBST EmptyBST x EmptyBST
insert2BST x (NodeBST lt e rt)
    | x == e = NodeBST lt e rt
    | x < e = insert2BST x lt
    | otherwise = insert2BST x rt

flattenBT :: BinTree a -> [a]
flattenBT (Leaf x) = [x]
flattenBT (NodeBT lt rt) = (flattenBT lt) ++ (flattenBT rt)