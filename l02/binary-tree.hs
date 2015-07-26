module Tree where

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

data MTree a = MNode a (Maybe (MTree a)) (Maybe (MTree a))
             deriving (Show)


treeHeight :: Integral b => Tree a -> b
treeHeight Empty        = 0
treeHeight (Node _ a b) = 1 + max (treeHeight a) (treeHeight b)
