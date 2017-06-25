{-# LANGUAGE TypeSynonymInstances #-}

module Genetic where

import Control.Monad.Random
import Control.Monad (replicateM)
import Data.Set as S
import qualified Data.Vector as V
import Data.List as L
import Debug.Trace

type Genome a = V.Vector a
type Population a = V.Vector (Genome a)
data Goal = Minimize | Maximize
type Elitism = Int

data GeneticProblem a = GeneticProblem
    { goal :: Goal
    , elitism :: Elitism
    }

class Ord a => Gene a where
    disturb :: a -> a
    fitness :: Genome a -> Float
    isValid :: Genome a -> Bool
    rndGenome :: (RandomGen g) => Rand g (Genome a)

generationsSize :: Int
generationsSize = 100

populationSize :: Int
populationSize = 50

generatePopulation :: (RandomGen g, Gene a) => Int -> Rand g (Population a)
generatePopulation n = replicateM n rndGenome >>= return . V.fromList

solution :: (RandomGen g, Gene a)
    => GeneticProblem a
    -> Rand g Float
solution p = do
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

tournament :: (RandomGen g, Gene a)
    => GeneticProblem a
    -> Population a
    -> Rand g (Genome a)
tournament problem@(GeneticProblem Maximize _) p = tournament' problem maximum p
tournament problem@(GeneticProblem Minimize _) p = tournament' problem minimum p

tournament' (GeneticProblem Maximize _) best p = do
    let k = (div (V.length p) 4) + 2
    contestantsIndx <- replicateM k (getRandomR (0, (V.length p) - 1))
    let contestants = fmap (p V.!) contestantsIndx
    let fitnesses = fmap fitness contestants
    return $ snd $ best $ zip fitnesses contestants


mating :: (RandomGen g, Gene a) => Genome a -> Genome a -> Rand g (Genome a)
mating p1 p2 = crossover p1 p2 >>= mutate

crossover :: (RandomGen g, Gene a) => Genome a -> Genome a -> Rand g (Genome a)
crossover g1 g2 = do
    let n = V.length g1
        m = n `div` 2
        g1' = V.slice 0 m g1
        g2' = V.slice m (n-m) g2
        cross = g1' V.++ g2'
    if isValid cross then return cross else mutate g2 >>= crossover g1

mutate :: (RandomGen g, Gene a) => Genome a -> Rand g (Genome a)
mutate genome = do
    rndIndex <- getRandomR (0, (V.length genome) - 1)
    let rndGene = genome V.! rndIndex
    let mutant = genome V.// [(rndIndex, disturb rndGene)]
    if isValid mutant then return mutant else mutate genome
