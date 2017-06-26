module Main where

import Genetic
import Knapsack
import Tsp
import Control.Monad.Random
import Debug.Trace
import qualified Data.Vector as V

main :: IO ()
main = do
    -- sol <- evalRandIO $ solve tspProblem
    sol <- evalRandIO $ solve knapsackProblem
    print sol

-- main :: IO ()
-- main = do
--     sol <- evalRandIO $ solve knapsackProblem
--     print sol

    -- Test population generation
    -- pop <- (evalRandIO $ generatePopulation 50) :: IO (Population Element)
    -- print $ length pop
    -- print $ fmap knapsackIsUnderFilled pop
    -- print $ fmap knapsackTotalValue pop

    -- -- Test mutation
    -- genome <- evalRandIO $ knapsackRndSol >>= mutate
    -- print $ knapsackIsUnderFilled genome
    -- print $ knapsackTotalValue genome

    -- -- Test Crossover
    -- g1 <- evalRandIO $ knapsackRndSol
    -- g2 <- evalRandIO $ knapsackRndSol
    -- cross <- evalRandIO $ crossover g1 g2
    -- print cross
    -- print $ knapsackIsUnderFilled cross
    -- print $ knapsackTotalValue cross

    -- -- Test tournament
    -- pop <- (evalRandIO $ generatePopulation 50) :: IO (Population Element)
    -- selected <- evalRandIO $ tournament knapsackProblem pop
    -- print selected
    -- print $ knapsackIsUnderFilled selected
    -- print $ knapsackTotalValue selected

    -- -- Test generation
    -- pop <- (evalRandIO $ generatePopulation 50) :: IO (Population Element)
    -- gen <- evalRandIO $ generation knapsackProblem pop
    -- print $ sum $ fmap knapsackTotalValue pop
    -- print $ sum $ fmap knapsackTotalValue gen
