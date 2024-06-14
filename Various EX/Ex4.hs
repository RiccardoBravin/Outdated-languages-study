--implementing another monad
square x = x^2
add1 x = x+1

y = add1 $ square 2

--what if we want to keep track of the computations?

data NumberWithLog = NumberWithLog {
    number :: Int,
    logs :: [String]
}deriving (Eq, Show)

square1 :: Int -> NumberWithLog
square1 x = NumberWithLog (x^2) ["squared " ++ show x ++ " to get " ++ show (x^2)]

addone :: NumberWithLog -> NumberWithLog
addone x = NumberWithLog (number x + 1) (logs x ++ ["added 1 to " ++ show (number x) ++ " to get " ++ show (number x + 1)])

--these are though bad implementations, because we have to manually define each combination of int and NumberWithLog

wrapWithLog :: Int -> NumberWithLog
wrapWithLog x = NumberWithLog x ["Created x with value " ++ show x]

square2 :: NumberWithLog -> NumberWithLog
square2 x = NumberWithLog (number x^2) (logs x ++ ["squared " ++ show (number x) ++ " to get " ++ show (number x^2)])

test1 = addone $ square2 $ wrapWithLog 2

--we could define another function (that is actually the monad) that takes a function and a NumberWithLog and returns a NumberWithLog
runWithLogs:: NumberWithLog -> (Int -> NumberWithLog) -> NumberWithLog
runWithLogs input transform = 
                    let newNumberWithLog = transform $ number input --take just the number (int) from the input and apply the function to it
                    in NumberWithLog (number newNumberWithLog) (logs input ++ logs newNumberWithLog) --create a new NumberWithLog with the transformed number and the logs of the input concatenated to the newNumberWithLog

addone1:: Int -> NumberWithLog
addone1 x = NumberWithLog (x+1) ["added 1 to " ++ show x ++ " to get " ++ show (x+1)]

a = wrapWithLog 2
b = runWithLogs a square1
c = runWithLogs b addone1
d = runWithLogs c square1

main = do
    print $ d
    print $ number d
    print $ logs d
