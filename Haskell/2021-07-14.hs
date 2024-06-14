-- 1) Define a "generalized" zip function which takes a finite list of possibly infinite lists, and returns a
-- possibly infinite list containing a list of all the first elements, followed by a list of all the second elements,
-- and so on.
-- E.g. gzip [[1,2,3],[4,5,6],[7,8,9,10]] ==> [[1,4,7],[2,5,8],[3,6,9]]
-- 2) Given an input like in 1), define a function which returns the possibly infinite list of the sum of the two
-- greatest elements in the same positions of the lists.
-- E.g. sum_two_greatest [[1,8,3],[4,5,6],[7,8,9],[10,2,3]] ==> [17,16,15]

--esercizio 1
gzip list =
    if any null list then --if any elment in list is a null we return the empty list
        []
    else --otherwhise we concatenate all first elements of the list with the remaning part to compute 
        (map head list) : gzip (map tail list)


-- esercizio 2

makeBiggestPair :: (Ord a) =>  a -> (a, a) -> (a, a)
makeBiggestPair new (a, b)
  | new > b = (b , new)
  | new > a = (new , b)
  | otherwise = (a , b)

topTwo (x:y:xs) = foldr makeBiggestPair (if x<y then (x,y) else (y,x)) xs


sum_two_greatest list = [ x + y | (x,y) <- map topTwo (gzip list) ] 
