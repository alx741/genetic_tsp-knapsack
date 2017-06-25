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

generationsSize :: Int
generationsSize = 1000

populationSize :: Int
populationSize = 50

-- class GeneticProblem a where
--     -- fitnessGoal :: Goal
--     elitism :: Elitism
--     fitnessValue :: Genome a -> Float

data GeneticProblem a = GeneticProblem
    { goal :: Goal
    , elitism :: Elitism
    }

class Ord a => Gene a where
    disturb :: a -> a
    fitness :: Genome a -> Float
    isValid :: Genome a -> Bool
    rndGenome :: (RandomGen g) => Rand g (Genome a)

generatePopulation :: (RandomGen g, Gene a) => Int -> Rand g (Population a)
generatePopulation n = replicateM n rndGenome >>= return . V.fromList

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

-- selectRandomGene :: (RandomGen g) => Genome a -> Rand g a
-- selectRandomGene genome = do
--     rndIndex <- getRandomR (0, (V.length genome) - 1)
--     return $ genome V.! rndIndex

-- data Fitness a = Fitness
--     { fitnessGoal :: Goal
--     , fitnessValue :: Genome a -> Float
--     }

-- data Problem a = Problem
--     { elitism :: Elitism
--     , fitness :: Fitness a
--     , isValid :: (Genome a -> Bool)
--     , disturb :: (a -> a)
--     }

-- class RndGenome a where
--     getRandomGenome :: (RandomGen g) => Rand g (Genome a)

-- instance RndGenome (Genome a) where
--     getRandomGenome = undefined

-- solution :: (RandomGen g, Ord a, RndGenome a)
--     => Problem a
--     -> Rand g Float
-- solution p = do
--     population <- trace "rndpop" (getRandomPopulation populationSize)
--     lastGen <- nthGeneration p generationsSize population
--     return $ best p lastGen
--     where
--         best (Problem _ (Fitness Maximize fitness) _ _) p =  maximum $ fmap fitness $ S.toList p
--         best (Problem _ (Fitness Minimize fitness) _ _) p =  minimum $ fmap fitness $ S.toList p


-- nthGeneration :: (RandomGen g, Ord a)
--     => Problem a
--     -> Int
--     -> Population a
--     -> Rand g (Population a)
-- nthGeneration _ 0 p = return p
-- nthGeneration pro n p = do
--     gen <- generation pro p
--     nthGeneration pro (n-1) gen
-- -- nthGeneration pro n p = generation pro p >>= nthGeneration pro (n-1)


-- generation :: (RandomGen g, Ord a)
--     => Problem a
--     -> Population a
--     -> Rand g (Population a)
-- generation pro@(Problem _ (Fitness Maximize _) _ _) p = generation' pro p S.toDescList
-- generation pro@(Problem _ (Fitness Minimize _) _ _) p = generation' pro p S.toAscList

-- generation' (Problem n fit isValid disturb) p flist = do
--     let elitePopulation = take n $ flist p
--     children <- replicateM m $ getChild fit isValid disturb p
--     return $ S.fromList $ L.union elitePopulation children
--     -- return $ S.union elitePopulation (S.fromList children)
--     -- return $ S.union ((trace ("elite: " ++ show (size elitePopulation))) elitePopulation)
--     --     $ trace ("children: " ++ show (size $ S.fromList children)) $ S.fromList children
--     where
--         m = (size p) - n
--         getChild fit isValid disturb p = do
--             p1 <- tournament fit p
--             p2 <- tournament fit p
--             mating isValid disturb p1 p2




-- getRandomGene :: (RandomGen g) => Genome a -> Rand g a
-- getRandomGene genome = do
--     rndIndex <- getRandomR (0, (size genome) - 1)
--     return $ S.elemAt rndIndex genome

-- getRandomGenes :: (RandomGen g) => Int -> Genome a -> Rand g [a]
-- getRandomGenes n genome = replicateM n $ getRandomGene genome
