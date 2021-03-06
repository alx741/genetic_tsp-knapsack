{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Knapsack where

import Genetic
import Control.Monad.Random
import qualified Data.Vector as V

knapsackSize = 50

data Element = Element
    { elemId :: ElementId
    , selected :: Bool
    , weight :: Int
    , value :: Float
    }

newtype ElementId = ElementId Int
    deriving (Eq, Ord, Show, Read)

instance Eq Element where
    (==) e1 e2 = elemId e1 == elemId e2

instance Ord Element where
    (<=) e1 e2 = (value e1) <= (value e2)

instance Show Element where
    show (Element eid selected w v) =
        "Element:\n\tId: " ++ show eid ++ "\n\tSelected: " ++ show selected
            ++ "\n\tWeight: " ++ show w ++ "\n\tValue: " ++ show v ++ "\n"

instance Gene Element where
    fitness = knapsackTotalValue
    isValid = knapsackIsUnderFilled
    mutate = knapsackMutate
    crossover = knapsackCrossover
    rndGenome = knapsackRndSol

knapsackProblem :: GeneticProblem Element
knapsackProblem = GeneticProblem Maximize 1

knapsackMutate :: (RandomGen g) => Genome Element -> Rand g (Genome Element)
knapsackMutate genome = do
    rndIndex <- getRandomR (0, (V.length genome) - 1)
    let rndGene = genome V.! rndIndex
    let mutant = genome V.// [(rndIndex, knapsackSwap rndGene)]
    if isValid mutant then return mutant else mutate genome

knapsackTotalValue :: Genome Element -> Float
knapsackTotalValue genome = totalValue genome
    where totalValue gen =
            V.foldl' (\i g -> if selected g then i + value g else i) 0 gen

knapsackIsUnderFilled :: Genome Element -> Bool
knapsackIsUnderFilled genome = totalWeight genome <= knapsackSize
    where totalWeight gen =
            V.foldl' (\i g -> if selected g then i + weight g else i) 0 gen

knapsackCrossover :: (RandomGen g) => Genome Element -> Genome Element -> Rand g (Genome Element)
knapsackCrossover g1 g2 = do
    let n = V.length g1
        m = n `div` 2
        g1' = V.slice 0 m g1
        g2' = V.slice m (n-m) g2
        cross = g1' V.++ g2'
    if isValid cross then return cross else mutate g2 >>= knapsackCrossover g1

knapsackRndSol :: (RandomGen g) => Rand g (Genome Element)
knapsackRndSol = do
    rndElements <- mapM flipRandom elements
    let genome = V.fromList rndElements
    if isValid genome then return genome else knapsackRndSol
    where
        flipRandom e = do
            bool <- getRandom
            if bool then return $ knapsackSwap e else return $ e

knapsackSwap :: Element -> Element
knapsackSwap (Element eid selected w v) = Element eid (not selected) w v



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
