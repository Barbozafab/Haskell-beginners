{-# LANGUAGE InstanceSigs #-}

-- 1. Write a function that will display only the first three letters of a weekday.
data Weekday = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Show, Eq, Bounded, Enum)

toShortString :: Show a => a -> String
toShortString = take 3 . show

-- 2. Write a function that returns next day of the week, following the given day.
next :: (Eq a, Bounded a, Enum a) => a -> a
next enum = if enum == maxBound then minBound else succ enum

-- 3. Implement a function that calculates number of days from the first weekday to the second.
daysTo :: (Enum a1, Enum a2) => a1 -> a2 -> Int
daysTo enum1 enum2 = mod (fromEnum enum2 - fromEnum enum1 + 7) 7

-- 4. Implement 'Semigroup' instances for all types and 'Monoid'
--    instances if it's possible to have a lawful 'Monoid' instance.

newtype Gold = Gold {unGold :: Int} deriving (Show, Eq)

instance Semigroup Gold where
    (<>) :: Gold -> Gold -> Gold
    Gold a <> Gold b = Gold $ a + b

instance Monoid Gold where
    mempty :: Gold
    mempty = Gold 0

-- 

data Reward = Reward {rewardGold :: Gold, rewardSpecial :: Bool} deriving (Show, Eq)

instance Semigroup Reward where
    (<>) :: Reward -> Reward -> Reward
    a <> b = Reward (rewardGold a <> rewardGold b) (rewardSpecial a || rewardSpecial b)

instance Monoid Reward where
    mempty :: Reward
    mempty = Reward mempty False

-- 

data List1 a = List1 a [a] deriving (Show, Eq)

instance Semigroup (List1 a) where
    (<>) :: List1 a -> List1 a -> List1 a
    List1 a as <> List1 b bs = List1 a $ as <> (b : bs)

-- 

data Treasure a
    = NoTreasure
    | SomeTreasure a
    deriving (Show, Eq)

instance Semigroup a => Semigroup (Treasure a) where
    (<>) :: Treasure a -> Treasure a -> Treasure a
    a <> NoTreasure = a
    NoTreasure <> b = b
    SomeTreasure a <> SomeTreasure b = SomeTreasure $ a <> b

instance Monoid a => Monoid (Treasure a) where
    mempty :: Treasure a
    mempty = NoTreasure

-- 5. Implement a polymorphic function that takes three elements and appends together only different elements.
appendDiff3 :: (Semigroup a, Eq a) => a -> a -> a -> a
appendDiff3 a b c
    | a == b && b == c = a
    | a == b = a <> c
    | b == c = a <> b
    | a == c = b <> c
    | otherwise = a <> b <> c

-- 6. Implement 'Foldable' instances for all types that can have such an instance.
instance Foldable List1 where
    foldr :: (a -> b -> b) -> b -> List1 a -> b
    foldr f acc (List1 a as) = f a $ foldr f acc as

    foldMap :: Monoid m => (a -> m) -> List1 a -> m
    foldMap f = foldr ((<>) . f) mempty

-- 

instance Foldable Treasure where
    foldr :: (a -> b -> b) -> b -> Treasure a -> b
    foldr _ acc NoTreasure = acc
    foldr f acc (SomeTreasure a) = f a acc

    foldMap :: Monoid m => (a -> m) -> Treasure a -> m
    foldMap f = foldr ((<>) . f) mempty

-- 7. Implement 'Functor' instances for all types that can have such an instance.
instance Functor List1 where
    fmap :: (a -> b) -> List1 a -> List1 b
    fmap f (List1 a as) = List1 (f a) (fmap f as)

-- 

instance Functor Treasure where
    fmap :: (a -> b) -> Treasure a -> Treasure b
    fmap _ NoTreasure = NoTreasure
    fmap f (SomeTreasure a) = SomeTreasure $ f a

-- 8. Now, you have a function inside some 'Functor'. You're a given an element
--    and you need to apply the function inside the 'Functor' to a given element.
apply :: Functor f => a -> f (a -> b) -> f b
apply element = fmap (\f -> f element)

main :: IO()
main = do
    print $ toShortString Monday
    print $ next Monday
    print $ daysTo Friday Wednesday
    print $ Gold 1 <> Gold 2
    print $ Gold 1
    print $ Reward (Gold 1) False <> Reward (Gold 2) True
    print $ Reward (Gold 1) False
    print $ List1 1 [2, 3] <> List1 4 [5, 6]
    print $ SomeTreasure (Gold 1) <> SomeTreasure (Gold 2)
    print $ SomeTreasure $ Gold 1
    print $ appendDiff3 (Gold 1) (Gold 2) (Gold 2)
    print $ foldr (+) 1 (List1 2 [3, 4])
    print $ foldMap show (List1 1 [2, 3])
    print $ foldr (+) 1 (SomeTreasure 2)
    print $ foldMap show (SomeTreasure $ Gold 1)
    print $ fmap (+ 4) (List1 1 [2, 3])
    print $ fmap (<> Gold 2) (SomeTreasure $ Gold 1)
    print $ apply 5 (Just (+ 3))
