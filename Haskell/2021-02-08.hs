{--Ex 2
HASKELL:
A multi-valued map (Multimap) is a data structure that associates keys of
a type k to zero or more values of type v.  A Multimap can be represented as
a list of 'Multinodes', as defined below. Each multinode contains a unique key
and a non-empty list of values associated to it.

data Multinode k v = Multinode { key :: k
                               , values :: [v]
                               }

data Multimap k v = Multimap [Multinode k v]

1) Implement the following functions that manipulate a Multimap:

insert :: Eq k => k -> v -> Multimap k v -> Multimap k v
insert key val m returns a new Multimap identical to m, except val is added to the values associated to k.

lookup :: Eq k => k -> Multimap k v -> [v]
lookup key m returns the list of values associated to key in m

remove :: Eq v => v -> Multimap k v -> Multimap k v
remove val m returns a new Multimap identical to m, but without all values equal to val
--}

data Multinode k v = Multinode { key :: k, values :: [v]} deriving (Show)

data Multimap k v = Multimap [Multinode k v] deriving (Show) --list of multinodes where each multinode has key and list of values


empty = Multimap []

insert :: Eq k => k -> v -> Multimap k v -> Multimap k v
insert k v (Multimap []) = Multimap [Multinode k [v]]
insert k v (Multimap (m@(Multinode k' vals):mns))
    | k == k' = Multimap ((Multinode k' (v:vals)):mns)
    | otherwise = let Multimap p = insert k v (Multimap mns)
                    in Multimap ((Multinode k' vals):p)


lookup :: Eq k => k -> Multimap k v -> [v]
lookup _ (Multimap []) = []
lookup k (Multimap ((Multinode k' vals):mns))
    | k == k' = vals
    | otherwise = Main.lookup k (Multimap mns)

remove :: Eq v => v -> Multimap k v -> Multimap k v
remove _ (Multimap []) = Multimap []
remove v (Multimap ((Multinode k' vals):mns)) = let xs = removeL v vals
                                                in if (null xs)
                                                        then remove v (Multimap mns)
                                                        else let Multimap p = remove v (Multimap mns) 
                                                            in Multimap ((Multinode k' xs):p) 

removeL _ [] = []
removeL v (x:xs)
    | x == v =  removeL v xs
    | otherwise = x:(removeL v xs)



sos = insert 1 1 empty
sis = insert 1 2 sos
ses = insert 2 3 sis
sus = insert 2 1 ses