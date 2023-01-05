-- 1. Specify the type signature of the following function.
makeSnippet :: Int -> [Char] -> [Char]
makeSnippet limit text = take limit ("Description: " ++ text) ++ "..."

-- 2. Implement a function that takes two numbers and finds sum of their squares.
sumOfSquares :: Int -> Int -> Int
sumOfSquares x y = x ^ 2 + y ^ 2

-- 3. Implement a function that returns the last digit of a given number.
lastDigit :: Int -> Int
lastDigit n = mod (abs n) 10

-- 4. Write a function that takes three numbers and returns the
--    difference between the biggest number and the smallest one.
minmax :: Int -> Int -> Int -> Int
minmax x y z = maxNum - minNum
    where
        maxNum = max x $ max y z
        minNum = min x $ min y z

-- 5. Implement a function that takes a string, start and end positions and returns
--    a substring of given string from the start position to the end (including).
subString :: Int -> Int -> [Char] -> [Char]
subString start end str
    | end < 0 = ""
    | start < 0 = take (end + 1) str
    | otherwise = take (end - start + 1) (drop start str)

-- 6. Write a function that takes a String — space separated numbers,
--    and finds a sum of the numbers inside this string.
strSum :: [Char] -> Int
strSum str = sum $ map read $ words str

-- 7. Write a function that takes a number and a list of numbers and
--    returns a string, saying how many elements of the list are
--    strictly greater than the given number and strictly lower.
lowerAndGreater :: Int -> [Int] -> [Char]
lowerAndGreater n list = show n ++ " is lower than " ++ show (lower list) ++ " and greater than " ++ show (greater list) ++ " numbers of the list."
    where
        lower = foldl (\acc x -> if n < x then acc + 1 else acc) 0
        greater = foldl (\acc x -> if n > x then acc + 1 else acc) 0

main :: IO()
main = do
    print $ makeSnippet 25 ['a'..'z']
    print $ sumOfSquares 3 4
    print $ lastDigit 42
    print $ minmax 7 1 4
    print $ subString (-1) 7 "Hello, world!"
    print $ strSum "100    -42  15"
    print $ lowerAndGreater 3 [1 .. 9]
