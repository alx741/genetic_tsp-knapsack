{-# LANGUAGE TypeSynonymInstances #-}

module Genetic where

import Control.Monad.Random
import Control.Monad (replicateM)
import qualified Data.Vector as V

type Genome a = V.Vector a
type Population a = V.Vector (Genome a)
data Goal = Minimize | Maximize
type Elitism = Int

data GeneticProblem a = GeneticProblem
    { goal :: Goal
    , elitism :: Elitism
    }

class Ord a => Gene a where
    fitness :: Genome a -> Float
    isValid :: Genome a -> Bool
    mutate :: (RandomGen g) => Genome a -> Rand g (Genome a)
    crossover :: (RandomGen g) => Genome a -> Genome a -> Rand g (Genome a)
    rndGenome :: (RandomGen g) => Rand g (Genome a)

generationsSize :: Int
generationsSize = 500

populationSize :: Int
populationSize = 50

generatePopulation :: (RandomGen g, Gene a) => Int -> Rand g (Population a)
generatePopulation n = replicateM n rndGenome >>= return . V.fromList

solve :: (RandomGen g, Gene a)
    => GeneticProblem a
    -> Rand g Float
solve p = do
    population <- generatePopulation populationSize
    lastGeneration <- nthGeneration p generationsSize population
    return $ best p lastGeneration
    where
        best (GeneticProblem Maximize _) p =  V.maximum $ fmap fitness p
        best (GeneticProblem Minimize _) p =  V.minimum $ fmap fitness p

generation :: (RandomGen g, Gene a)
    => GeneticProblem a
    -> Population a
    -> Rand g (Population a)
generation pro@(GeneticProblem _ n) p = do
    let elitePopulation = V.take n p
    children <- V.replicateM m $ getChild pro p
    return $ elitePopulation V.++ children
    where
        m = (V.length p) - n
        getChild pro p = do
            p1 <- tournament pro p
            p2 <- tournament pro p
            mating p1 p2

nthGeneration :: (RandomGen g, Gene a)
    => GeneticProblem a
    -> Int
    -> Population a
    -> Rand g (Population a)
nthGeneration _ 0 p = return p
nthGeneration pro n p = do
    gen <- generation pro p
    nthGeneration pro (n-1) gen

mating :: (RandomGen g, Gene a) => Genome a -> Genome a -> Rand g (Genome a)
mating p1 p2 = crossover p1 p2 >>= mutate

tournament :: (RandomGen g, Gene a)
    => GeneticProblem a
    -> Population a
    -> Rand g (Genome a)
tournament problem@(GeneticProblem Maximize _) p = tournament' problem maximum p
tournament problem@(GeneticProblem Minimize _) p = tournament' problem minimum p

tournament' _ best p = do
    let k = (div (V.length p) 4) + 2
    contestantsIndx <- replicateM k (getRandomR (0, (V.length p) - 1))
    let contestants = fmap (p V.!) contestantsIndx
    let fitnesses = fmap fitness contestants
    return $ snd $ best $ zip fitnesses contestants
