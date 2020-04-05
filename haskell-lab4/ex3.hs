data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree
                  deriving (Eq, Ord, Show, Read)

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt

data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a | -- literal/value a, e.g. Lit 2 = 2
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfIntBT :: BinIntTree -> Int
depthOfIntBT EmptyIntBT = 0
depthOfIntBT (IntNodeBT n lt rt) = 1 + max (depthOfIntBT lt) (depthOfIntBT rt)

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT n lt rt) = 1 + max (depthOfBT lt) (depthOfBT rt)

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT n lt rt) = NodeBT(f n) (mapBT f lt) (mapBT f rt)

instance Eq a => Eq (BinTree a) where
    (==) (EmptyBT) (EmptyBT) = True
    (==) (EmptyBT) (list) = False
    (==) (list) (EmptyBT) = False
    (==) (NodeBT n1 lt1 rt1) (NodeBT n2 lt2 rt2) = n1 == n2