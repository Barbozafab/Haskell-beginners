{-# LANGUAGE InstanceSigs #-}

module Main where

import Text.Read (readMaybe)
import Data.Text (splitOn, pack, unpack)
import Data.Semigroup (Sum (Sum, getSum), Max (Max, getMax), Min (Min, getMin), Semigroup (sconcat))
import Data.List.NonEmpty (NonEmpty ((:|)), fromList)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

data TradeType
    = Buy
    | Sell
    deriving (Show, Eq, Read)

data Row = Row
    { rowProduct   :: String
    , rowTradeType :: TradeType
    , rowCost      :: Int
    } deriving (Show, Eq)

parseRow :: String -> Maybe Row
parseRow str = let parts = splitOn (pack ",") (pack str) in
    case length parts of
        3 -> do
            let newParts = map unpack parts
            let rP = head newParts
            let rTT = if newParts!!1 == "Buy" then Buy else Sell
            let rC = read $ newParts!!2
            Just Row {rowProduct=rP, rowTradeType=rTT, rowCost=rC}
        _ -> Nothing

newtype MaxLen = MaxLen
    { unMaxLen :: String
    } deriving (Show, Eq)

instance Semigroup MaxLen where
    (<>) :: MaxLen -> MaxLen -> MaxLen
    MaxLen a <> MaxLen b = if length a >= length b then MaxLen a else MaxLen b

data Stats = Stats
    { statsTotalPositions :: Sum Int
    , statsTotalSum       :: Sum Int
    , statsAbsoluteMax    :: Max Int
    , statsAbsoluteMin    :: Min Int
    , statsSellMax        :: Maybe (Max Int)
    , statsSellMin        :: Maybe (Min Int)
    , statsBuyMax         :: Maybe (Max Int)
    , statsBuyMin         :: Maybe (Min Int)
    , statsLongest        :: MaxLen
    } deriving (Show, Eq)

instance Semigroup Stats where
    (<>) :: Stats -> Stats -> Stats
    a <> b = Stats
        { statsTotalPositions = statsTotalPositions a <> statsTotalPositions b
        , statsTotalSum = statsTotalSum a <> statsTotalSum b
        , statsAbsoluteMax = statsAbsoluteMax a <> statsAbsoluteMax b
        , statsAbsoluteMin = statsAbsoluteMin a <> statsAbsoluteMin b
        , statsSellMax = statsSellMax a <> statsSellMax b
        , statsSellMin = statsSellMin a <> statsSellMin b
        , statsBuyMax = statsBuyMax a <> statsBuyMax b
        , statsBuyMin = statsBuyMin a <> statsBuyMin b
        , statsLongest = statsLongest a <> statsLongest b
        }

rowBaseStats :: Row -> Stats
rowBaseStats row = Stats
    { statsTotalPositions = 1
    , statsTotalSum = Sum 0
    , statsAbsoluteMax = Max $ rowCost row
    , statsAbsoluteMin = Min $ rowCost row
    , statsSellMax = Nothing
    , statsSellMin = Nothing
    , statsBuyMax = Nothing
    , statsBuyMin = Nothing
    , statsLongest = MaxLen $ rowProduct row
    }

rowToBuy :: Row -> Stats -> Stats
rowToBuy row stats = stats
    { statsTotalSum = Sum $ rowCost row * (-1)
    , statsBuyMax = Just $ Max $ rowCost row
    , statsBuyMin = Just $ Min $ rowCost row
    }

rowToSell :: Row -> Stats -> Stats
rowToSell row stats = stats
    { statsTotalSum = Sum $ rowCost row
    , statsBuyMax = Just $ Max $ rowCost row
    , statsBuyMin = Just $ Min $ rowCost row
    }

rowToStats :: Row -> Stats
rowToStats row = if rowTradeType row == Buy then rowToBuy row (rowBaseStats row) else rowToSell row (rowBaseStats row)

combineRows :: NonEmpty Row -> Stats
combineRows (row :| []) = rowToStats row
combineRows (row :| (row' : rows)) = rowToStats row <> combineRows (row' :| rows)

displayStats :: Stats -> String
displayStats stats = do
    let tp = "Total positions:       : " ++ show (getSum (statsTotalPositions stats)) ++ ['\n']
    let tfb = "Total final balance    : " ++ show (getSum (statsTotalSum stats)) ++ ['\n']
    let bac = "Biggest absolute cost  : " ++ show (getMax (statsAbsoluteMax stats)) ++ ['\n']
    let sac = "Smallest absolute cost : " ++ show (getMin (statsAbsoluteMin stats)) ++ ['\n']
    let maxe = "Max earning            : " ++ maybe "no value" (show . getMax) (statsSellMax stats) ++ ['\n']
    let mine = "Min earning            : " ++ maybe "no value" (show . getMin) (statsSellMin stats) ++ ['\n']
    let maxs = "Max spending           : " ++ maybe "no value" (show . getMax) (statsBuyMax stats) ++ ['\n']
    let mins = "Min spending           : " ++ maybe "no value" (show . getMin) (statsBuyMin stats) ++ ['\n']
    let lpn = "Longest product name   : " ++ unMaxLen (statsLongest stats) ++ ['\n']
    tp ++ tfb ++ bac ++ sac ++ maxe ++ mine ++ maxs ++ mins ++ lpn

calculateStats :: String -> String
calculateStats content = displayStats $ combineRows $ fromList $ mapMaybe parseRow $ lines content

printProductStats :: FilePath -> IO ()
printProductStats path = do
    content <- readFile path
    putStrLn $ calculateStats content

main :: IO ()
main = do
    args <- getArgs
    printProductStats $ head args
