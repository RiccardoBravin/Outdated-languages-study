import Data.Map
import Data.Array

--declare a function
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

--declare new types
data MyBool = MyTrue | MyFalse

--declare a new type which is a tuple of two elements and has default accessors for x and y
data CartesianPoint a = CartesianPoint {pointx, pointy :: a} --like a struct Pnt<A> {A x; A y;} in cpp
pointInstance = CartesianPoint 1.12 2.34
--to access the elements of the tuple, use the accessors like: pointx pointInstance

data Tree a = Leaf a | Branch (Tree a) a (Tree a) 
--Branch :: Tree a -> a -> Tree a -> Tree a

treeInstance = Branch (Leaf 'a') 'b' (Branch (Leaf 'c') 'd' (Leaf 'e'))

--declare a function that takes a tree and returns a list
treeToList :: Tree a -> [a]
treeToList (Leaf x) = [x]
treeToList (Branch left x right) = treeToList left ++ [x] ++ treeToList right -- ++ is for list concatenation

--define a synonim for a type
type String = [Char]

--map function (takes a function and a list and returns a list)
myMap :: (t -> a) -> [t] -> [a]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

myLambda = (\x -> myMap (+ 1) [x, x+1, x+2])

--infinite computations 
squares = Prelude.map (^2) [0..] --list of squares of all numbers

--zip function
ziplist = zip [1,2,3,4,5,6,7,8] "ciao" --this will be [(1,'c'),(2,'i'),(3,'a'),(4,'o')]

--list comprehension
cartProduct = [(x,y) | x <- ["Dio", "Gesu"], y <- ["Porco", "Cane", "Cristo", "Madonna", "Infame"]]

--infinite computation with list comprehension
cartProduct2 = [(x,y) | x <- [1..], y <- [1..]]
fib = 1 : 1 : [a+b | (a,b) <- zip fib (tail fib)]

isPrime n = not(any (\x -> mod n x == 0) (takeWhile (\x -> x^2 <= n) primeList))
primeList = 2:[x | x <- [3,5..], isPrime x] 

--pattern matching
sign x | x > 0 = 1
       | x == 0 = 0
       | otherwise = -1

myTake m ys = case (m,ys) of
    (0,_) -> [] -- _ is a wildcard for any value
    (_,[]) -> [] -- the order of the cases is important since the explicit values are checked first for equivalence and thus need to be computable
    (n,x:xs) -> x : myTake (n-1) xs


--if statement is defined as a normal function 
myIf True  x _ = x
myIf False _ y = y

--let statement
pippo = let x = 3
            y = 4
        in x*y -- returns 12 

pluto = let {x = 3; y = 4} in x*y

--where statement
paperino = x*y
    where x = 3
          y = 4

powset set = powset' set [[]] where --definisco una funzione stub interna
                powset' [] out = out
                powset' (e:set) out = powset' set (out ++ [e:x | x <- out ]) --tolgo un elemento e aggiungo a out tutte le concatenazioni 
                                                                             --fatte con il numero estratto e le liste già in out 


myFoldl f z l = case (f, z, l) of
                    (_, z, []) -> z
                    (f, z, x:xs) -> let z' = f z x 
                                        in seq z' (myFoldl f z' xs) --seq is used to force the evaluation of z' before the recursive call 


--instances
instance (Eq a) => Eq (Tree a) where --with (Eq a) we indicate that a must implement the eq operator
    Leaf a == Leaf b = a == b
    (Branch l1 x r1) == (Branch l2 y r2) = (y == x) && (l1 == l2) && (r1 == r2)
    _ == _ = False --any other combination of data constructors is not equal


instance (Show a) => Show (Tree a) where
    show (Leaf a) = show a
    show (Branch l x r) = "(" ++  show l ++ "_" ++ show x ++ "_" ++ show r ++ ")"

--automatic derivation of instances
infixr 5 :^:
data Tr a = Lf a | Tr a :^: Tr a deriving (Eq, Show)

tr1 = Lf 1 :^: Lf 2
tr2 = Lf 1 :^: (Lf 2 :^: Lf 3)

--Ord instance
data RPS = Rock | Paper | Scissors deriving (Eq, Show)

instance Ord RPS where
    x <= y | x == y = True
    Rock <= Paper = True
    Paper <= Scissors = True
    Scissors <= Rock = True
    _ <= _ = False

--implementation of rational numbers
data Rat = Rat !Integer !Integer deriving(Eq)  -- ! is used to force the evaluation of the value
simplify (Rat x y) = let g = gcd x y in Rat (div x g) (div y g) --gcd is the greatest common divisor and div is the integer division
makeRat x y = simplify (Rat x y)

instance Num Rat where --sum, sub, mult, abs, signum and fromInteger are strictly necessary to be part of Num
    (Rat x y) + (Rat x' y') = makeRat (x*y' + x'*y) (y*y')
    (Rat x y) - (Rat x' y') = makeRat (x*y' - x'*y) (y*y')
    (Rat x y) * (Rat x' y') = makeRat (x*x') (y*y')
    abs (Rat x y) = makeRat (abs x) (abs y)
    signum (Rat x y) = makeRat (signum x * signum y) 1
    fromInteger x = makeRat x 1

instance Show Rat where
    show (Rat x y) = (show x) ++ "/" ++ (show y)

instance Ord Rat where
    (Rat x y) <= (Rat x' y') = x*y' <= x'*y


--map 
exmap = let m = fromList[("nose", 11), ("Diamond", 41)] --converts list of pairs to a map
            n = insert "rug" 98 m  --insert (key, value) in map m
            o = insert "Pippo" 42 n --insert (key, value) in map n    
        in (m Data.Map.! "Diamond", o Data.Map.! "Pippo", o Data.Map.! "nose" )
        --takes values with key x

exarr = let m = listArray (1,3) ["alpha", "beta", "Gamma"]
            n = m // [(2, "Beta")]
            o = n // [(1, "Alpha"), (3, "Delta")]
        in (m Data.Array.! 1, n Data.Array.! 3, o Data.Array.! 2, o Data.Array.! 3)


main :: IO ()
main = do
    print $ myLength [1,2,3,4,10]
    print $ pointx pointInstance
    print $ treeToList treeInstance
    print $ myMap (+2) [1,2,3,4,5]
    print $ myMap (myLambda . (-1 +) ) [1,2,3,4,5] --function composition
    print $ ziplist
    print $ Prelude.take 10 fib
    print $ primeList !! 10
    print $ powset [1,2,3]
    print $ myFoldl (+) 0 [1,2,3,4,5, 6, 7]
    print $ treeInstance
    print $ tr1 == tr2
    print $ tr2
    print $ exmap
    print $ exarr

