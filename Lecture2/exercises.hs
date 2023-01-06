import Data.Char (isSpace)
import Data.Maybe (isNothing, fromJust)
import Data.Either (fromRight, isLeft)

-- 1. Implement a function that finds a product of all the numbers in the list.
lazyProduct :: [Int] -> Int
lazyProduct list = case list of
    [] -> 0
    [x] -> x
    0 : _ -> 0
    x : xs -> x * lazyProduct xs

-- 2. Implement a function that duplicates every element in the list.
duplicate :: [a] -> [a]
duplicate = concatMap (replicate 2)

-- 3. Implement function that takes index and a list and removes the element at the given position.
removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt x list
    | x >= length list = (Nothing, list)
    | otherwise = (Just (list !! x), take x list ++ drop (x + 1) list)

-- 4. Write a function that takes a list of lists and returns only lists of even lengths.
-- evenLists :: [[a]] -> [[a]]
evenLists :: [[a]] -> [[a]]
evenLists = filter (even . length)

-- 5. Write a function that removes all leading and trailing spaces from a string
dropSpaces :: String -> String
dropSpaces = dropSpaces' . dropSpaces'
    where
        dropSpaces' = reverse . dropWhile isSpace

-- 6. Implement data types to describe treasure, knight and dragon.
--    And implement a function that takes a knight and a dragon and returns
--    one of the three possible fight outcomes.
newtype Health = MkHealth Int
newtype Attack = MkAttack Int
newtype Endurance = MkEndurance Int

data Knight = MkKnight
    { knightHealth    :: !Health
    , knightAttack    :: !Attack
    , knightEndurance :: !Endurance
    }

data Colour
    = Red
    | Black
    | Green

data Dragon = MkDragon
    { dragonColour    :: !Colour
    , dragonHealth    :: !Health
    , dragonFirePower :: !Attack
    }

dragonFight :: Knight -> Dragon -> String
dragonFight knight dragon = turn 1 knight dragon
    where
        turn :: Int -> Knight -> Dragon -> String
        turn count knight dragon
            | leqZeroHealth (dragonHealth dragon) = victory (dragonColour dragon)
            | leqZeroHealth (knightHealth knight) = "The dragon killed the knight!"
            | leqZeroEndurance (knightEndurance knight) = "The knight got exausted and ran away!"
            | otherwise = if mod count 10 == 0 then turn (count + 1) (breath knight dragon) (strike knight dragon) else turn (count + 1) knight (strike knight dragon)

        leqZeroHealth :: Health -> Bool
        leqZeroHealth (MkHealth health) = health <= 0

        leqZeroEndurance :: Endurance -> Bool
        leqZeroEndurance (MkEndurance endurance) = endurance <= 0

        breath :: Knight -> Dragon -> Knight
        breath knight dragon = updateKnight knight (knightHealth knight) (dragonFirePower dragon) (knightEndurance knight)

        updateKnight :: Knight -> Health -> Attack -> Endurance -> Knight
        updateKnight knight (MkHealth health) (MkAttack attack) (MkEndurance endurance) = knight {knightHealth = MkHealth (health - attack), knightEndurance = MkEndurance (endurance - 1)}

        strike :: Knight -> Dragon -> Dragon
        strike knight dragon = updateDragon dragon (dragonHealth dragon) (knightAttack knight)

        updateDragon :: Dragon -> Health -> Attack -> Dragon
        updateDragon dragon (MkHealth health) (MkAttack attack) = dragon {dragonHealth = MkHealth (health - attack)}

        victory :: Colour -> String
        victory colour = case colour of
            Red -> "The knight defeated the dragon and earned 100 XP points, gold and treasure!"
            Black -> "The knight defeated the dragon and earned 150 XP points, gold and treasure!"
            Green -> "The knight defeated the dragon and earned 250 XP points and gold!"

-- 7. Write a function that takes a list of numbers and returns 'True' if the list is sorted in ascending order
isIncreasing :: [Int] -> Bool
isIncreasing [] = True
isIncreasing [x] = True
isIncreasing (x : y : xs) = x <= y && isIncreasing xs

-- 8. Implement a function that takes two lists, sorted in the increasing order,
--    and merges them into new list, also sorted in the increasing order.
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys) = if x < y then x : merge xs (y : ys) else y : merge (x : xs) ys

-- 9. Implement the "Merge Sort" algorithm in Haskell.
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge (mergeSort (take len list)) (mergeSort (drop len list))
    where
        len = div (length list) 2

-- 10. Implement the addition of expressions
data Expr
    = Lit !Int
    | Var !String
    | Add !Expr !Expr
    deriving (Show, Eq)

type Variables = [(String, Int)]

newtype EvalError = VariableNotFound String deriving (Show, Eq)

eval :: Variables -> Expr -> Either EvalError Int
eval _ (Lit num)   = Right num
eval vars (Var str)
    | isNothing value = Left (VariableNotFound str)
    | otherwise = Right (fromJust value)
    where
        value = lookup str vars
eval vars (Add exp1 exp2)
    | isLeft eval1 = eval1
    | isLeft eval2 = eval2
    | otherwise = Right (fromRight 0 eval1 + fromRight 0 eval2)
    where
        eval1 = eval vars exp1
        eval2 = eval vars exp2

constantFolding :: Expr -> Expr
constantFolding exp = if acc == 0 then folded else Add folded (Lit acc)
    where
        (acc, folded) = constantFolding' 0 exp
        constantFolding' _ (Lit num) = (0, Lit num)
        constantFolding' acc (Var str) = (acc, Var str)
        constantFolding' acc (Add (Lit x) (Lit y)) = (acc, Lit (x + y))
        constantFolding' acc (Add (Lit x) exp) = constantFolding' (acc + x) exp
        constantFolding' acc (Add exp (Lit x)) = constantFolding' (acc + x) exp
        constantFolding' acc (Add exp1 exp2) = let
            (acc1, new1) = constantFolding' acc exp1
            (acc2, new2) = constantFolding' acc exp2
            in (acc1 + acc2, Add new1 new2)

main :: IO()
main = do
    print $ lazyProduct [4, 3, 7]
    print $ duplicate [3, 1, 2]
    print $ removeAt 0 [1 .. 5]
    print $ evenLists [[1,2,3], [3,1,2,7], [], [5, 7, 2]]
    print $ dropSpaces "   hello   "
    print $ dragonFight (MkKnight {knightHealth=MkHealth 30, knightAttack=MkAttack 10, knightEndurance=MkEndurance 25}) (MkDragon {dragonColour=Red, dragonHealth=MkHealth 50, dragonFirePower=MkAttack 25})
    print $ isIncreasing [3, 1, 2]
    print $ merge [1, 2, 4] [3, 7]
    print $ mergeSort [3, 1, 2]
    print $ eval [("x", 3), ("y", 4)] (Add (Var "x") (Add (Var "y") (Lit 5)))
    print $ eval [("x", 3), ("y", 4)] (constantFolding (Add (Add (Lit 2) (Var "x")) (Add (Var "y") (Lit 5))))
