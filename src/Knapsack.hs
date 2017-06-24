{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Knapsack where

import Genetic
import Control.Monad.Random
import Data.Set as S
import qualified Data.Vector as V
import Debug.Trace

knapsackSize = 50

data Element = Element
    { elemId :: ElementId
    , selected :: Bool
    , weight :: Int
    , value :: Int
    } deriving (Read)

newtype ElementId = ElementId Int
    deriving (Eq, Ord, Show, Read)

instance Eq Element where
    -- (==) e1 e2 = (elemId e1 == elemId e2) && (selected e1 == selected e2)
    (==) e1 e2 = elemId e1 == elemId e2

instance Ord Element where
    (<=) e1 e2 = (value e1) <= (value e2)

instance Show Element where
    show (Element eid selected w v) =
        "Element:\n\tId: " ++ show eid ++ "\n\tSelected: " ++ show selected
            ++ "\n\tWeight: " ++ show w ++ "\n\tValue: " ++ show v ++ "\n"

instance Gene Element where
    disturb = knapsackSwap
    fitness = knapsackTotalValue
    isValid = knapsackIsUnderFilled
    rndGenome = knapsackRndGenome

knapsackSwap :: Element -> Element
knapsackSwap (Element eid selected w v) = Element eid (not selected) w v

knapsackTotalValue :: Genome Element -> Float
knapsackTotalValue genome = fromIntegral $ totalValue genome
    where
        totalValue gen = V.foldl'
            (\i g -> if selected g then i + value g else i) 0 gen

knapsackIsUnderFilled :: Genome Element -> Bool
knapsackIsUnderFilled genome = totalWeight genome <= knapsackSize
    where
        totalWeight gen = V.foldl' (\i g -> if selected g then i + weight g else i) 0 gen

knapsackRndGenome :: (RandomGen g) => Rand g (Genome Element)
knapsackRndGenome = do
    -- rndElements <- trace "rndgenome" (mapM flipRandom elements)
    rndElements <- mapM flipRandom elements
    return $ V.fromList rndElements
    -- TODO: Assert genomes are valid
    where
        flipRandom e = do
            bool <- getRandom
            if bool then return $ disturb e else return $ e

-- knapsackProblem :: Problem Element
-- knapsackProblem = Problem 5 knapsackFitness knapsackIsValid knapsackDisturb

-- instance RndGenome Element where
--     getRandomGenome = do
--         -- rndElements <- trace "rndgenome" (mapM flipRandom elements)
--         rndElements <- mapM flipRandom elements
--         return $ S.fromList rndElements
--         where
--             flipRandom e = do
--                 bool <- getRandom
--                 if bool then return $ knapsackDisturb e else return $ e

-- Total Weight = 70
-- Total Value = 73
elements :: [Element]
elements =
    [ Element (ElementId 1) False 5 5
    , Element (ElementId 2) False 4 8
    , Element (ElementId 3) False 2 1
    , Element (ElementId 4) False 3 7
    , Element (ElementId 5) False 4 4
    , Element (ElementId 6) False 5 9
    , Element (ElementId 7) False 9 3
    , Element (ElementId 8) False 8 7
    , Element (ElementId 9) False 1 3
    , Element (ElementId 10) False 7 3
    , Element (ElementId 11) False 1 2
    , Element (ElementId 12) False 8 4
    , Element (ElementId 13) False 3 1
    , Element (ElementId 14) False 4 9
    , Element (ElementId 15) False 6 7
    ]
