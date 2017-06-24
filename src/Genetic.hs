{-# LANGUAGE TypeSynonymInstances #-}

module Genetic where

import Control.Monad.Random
import Control.Monad (replicateM)
import Data.Set as S
import Data.Vector as V
import Data.List as L
import Debug.Trace

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- type Genome a = Set a
type Genome a = Vector a
type Population a = Vector (Genome a)
data Goal = Minimize | Maximize
type Elitism = Int

-- class GeneticProblem a where
--     -- fitnessGoal :: Goal
--     elitism :: Elitism
--     fitnessValue :: Genome a -> Float

data GeneticProblem a = GeneticProblem
    { goal :: Goal
    , elitism :: Elitism
    }

class Gene a where
    disturb :: a -> a
    fitness :: Genome a -> Float
    isValid :: Genome a -> Bool
    rndGenome :: (RandomGen g) => Rand g (Genome a)


-- mutation :: (RandomGen g, Gene a) => Genome a -> Rand g (Genome a)
-- mutation genome = do
--     rndGene <- selectRandomGene genome
--     let disturbedGene = disturb rndGene
--     let mutant =  S.insert disturbedGene $ S.delete rndGene genome
--     if isValid mutant
--         then return mutant
--         else mutation isValid disturb genome
-- -- mutation isValid disturb genome = do
-- --     rndGene <- getRandomGene genome
-- --     let disturbedGene = disturb rndGene
-- --     let mutant =  S.insert disturbedGene $ S.delete rndGene genome
-- --     if isValid mutant
-- --         then return mutant
-- --         else mutation isValid disturb genome

selectRandomGene :: (RandomGen g) => Genome a -> Rand g a
selectRandomGene genome = do
    rndIndex <- getRandomR (0, (V.length genome) - 1)
    return $ genome V.! rndIndex




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

generationsSize :: Int
generationsSize = 1000

populationSize :: Int
populationSize = 50

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


-- getRandomPopulation :: (RandomGen g, Ord a, RndGenome a) => Int -> Rand g (Population a)
-- getRandomPopulation n = replicateM n getRandomGenome >>= trace "Rnd Pop" (return . S.fromList)

-- mating :: (RandomGen g, Ord a)
--     => (Genome a -> Bool)
--     -> (a -> a)
--     -> Genome a
--     -> Genome a
--     -> Rand g (Genome a)
-- mating isValid disturb p1 p2 = do
--     child <- crossover isValid p1 p2
--     mutation isValid disturb child


-- crossover :: (RandomGen g, Ord a)
--     => (Genome a -> Bool)
--     -> Genome a
--     -> Genome a
--     -> Rand g (Genome a)
-- crossover isValid p1 p2 = do
--     genes <- getRandomGenes (size p1) $ S.union p1 p2
--     let offspring = S.fromList genes
--     if isValid offspring
--         then return offspring
--         else crossover isValid p1 p2


-- tournament :: (RandomGen g, Ord a)
--     => Fitness a
--     -> Population a
--     -> Rand g (Genome a)
-- tournament fit@(Fitness Maximize fitness) p = tournament' fit maximum p
-- tournament fit@(Fitness Minimize fitness) p = tournament' fit minimum p

-- tournament' (Fitness _ fitness) best p = do
--     let k = (div (size p) 4) + 2
--     contestantsIndx <- replicateM k (getRandomR (0, (size p) - 1))
--     let contestants = fmap (flip elemAt p) contestantsIndx
--     let fitnesses = fmap fitness contestants
--     return $ snd $ best $ zip fitnesses contestants


-- getRandomGene :: (RandomGen g) => Genome a -> Rand g a
-- getRandomGene genome = do
--     rndIndex <- getRandomR (0, (size genome) - 1)
--     return $ S.elemAt rndIndex genome

-- getRandomGenes :: (RandomGen g) => Int -> Genome a -> Rand g [a]
-- getRandomGenes n genome = replicateM n $ getRandomGene genome
