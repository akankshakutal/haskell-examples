data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Eq,Show)

instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

insert :: (Ord a)=> Tree a -> a ->Tree a
insert EmptyTree value = (Node value EmptyTree EmptyTree)
insert (Node x l r) value = if (value < x)
                            then Node x (insert l value) r
                            else Node x  l (insert r value)

inOrderTraverse :: Tree a -> [a]
inOrderTraverse (EmptyTree) = []
inOrderTraverse (Node x l r) = concat [inOrderTraverse l, [x],inOrderTraverse r]

preOrderTraverse :: Tree a -> [a]
preOrderTraverse (EmptyTree) = []
preOrderTraverse (Node x l r) = concat [[x], preOrderTraverse l, preOrderTraverse r]

postOrderTraverse :: Tree a -> [a]
postOrderTraverse (EmptyTree) = []
postOrderTraverse (Node x l r) = concat [postOrderTraverse l, postOrderTraverse r,[x]]

dfs ::(Eq a) => Tree a -> a -> Bool
dfs EmptyTree value = False
dfs tree value = elem value (inOrderTraverse tree)

getTreeNodes :: Tree a -> [Tree a]
getTreeNodes (Node x EmptyTree EmptyTree) = []
getTreeNodes (Node _ EmptyTree r) = [r]
getTreeNodes (Node _ l EmptyTree) = [l]
getTreeNodes (Node _ l r) = [l,r]

nodeValue :: Tree a ->  a
nodeValue (Node a _ _ ) = a;
 
bft :: [Tree a] -> [a]
bft [] = []
bft xs = map nodeValue xs ++ bft (concatMap getTreeNodes xs)

bfTraverse :: Tree a -> [a]
bfTraverse EmptyTree = []
bfTraverse tree = bft [tree]

bfs :: (Eq a) => Tree a -> a -> Bool
bfs EmptyTree value = False
bfs tree value = elem value (bfTraverse tree)

delete :: (Ord a) => Tree a -> a -> Tree a
delete EmptyTree value = EmptyTree
delete (Node x l  r) value  
 | value == x = deleteX (Node x l r)
 | value < x = Node x (delete l value) r
 | value > x = Node x l (delete r value)

leftistElement :: (Ord a) => Tree a -> a
leftistElement (Node x EmptyTree  _) = x
leftistElement (Node _ l _ ) = leftistElement l

deleteX :: (Ord a) => Tree a -> Tree a 
deleteX (Node x EmptyTree r) = r
deleteX (Node x l EmptyTree) = l
deleteX (Node x l r) = (Node y l r) 
 where 
 y = leftistElement r
