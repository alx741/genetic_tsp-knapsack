module Main where

import Genetic
import Knapsack
import Control.Monad.Random
import Debug.Trace
import Data.Set as S

main :: IO ()
main = do
    print ""
    -- sol <- evalRandIO $ trace "solving" (Genetic.solution knapsackProblem)
    -- print sol
