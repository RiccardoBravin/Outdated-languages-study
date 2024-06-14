import Control.Monad.State
{--HASKELL

Consider the following data structure for general binary trees:
data Tree a = Empty | Branch (Tree a) a (Tree a) deriving (Show, Eq)

Using the State monad as seen in class:
1) Define a monadic map for Tree, called mapTreeM.
2) Use mapTreeM to define a function which takes a tree and returns a tree containing list of elements that are all the data found in the original tree in a depth-first visit.

E.g.
From the tree: (Branch (Branch Empty 1 Empty) 2 (Branch (Branch Empty 3 Empty) 4 Empty))
we obtain:
Branch (Branch Empty [1] Empty) [1,2] (Branch (Branch Empty [1,2,3] Empty) [1,2,3,4] Empty)
--}

data Tree a = Empty | Branch (Tree a) a (Tree a) deriving (Show, Eq)

t = Branch (Branch Empty 1 Empty) 2 (Branch Empty 3 (Branch Empty 4 Empty))

--monadic function definition
mapTreeM f Empty = return Empty
mapTreeM f (Branch ls e rs) = do
    lt <- mapTreeM f ls
    e' <- f e
    rt <- mapTreeM f rs
    return (Branch lt e' rt)


depthTree t = let (StateT f) = mapTreeM
                                (\v -> do
                                        cur <- getState
                                        putState $ cur ++ [v]
                                        getState
                                )
                                t
               in snd $ f []

