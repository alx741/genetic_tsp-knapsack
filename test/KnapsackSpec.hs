module KnapsackSpec where

import Knapsack
import Genetic
import Test.Hspec
import qualified Data.Vector as V

spec :: Spec
spec = do
    describe "knapsackTotalValue" $ do
        it "calculates the fitness of a genome" $ do
            knapsackTotalValue emptyKnapsack `shouldBe` 0.0
            knapsackTotalValue exampleKnapsack `shouldBe` 18.0

    describe "knapsackIsUnderFilled" $ do
        it "tells if a knapsack is under filled" $ do
            knapsackIsUnderFilled exampleKnapsack `shouldBe` True
            knapsackIsUnderFilled overFilledKnapsack `shouldBe` False


exampleKnapsack :: Genome Element
exampleKnapsack = V.fromList $
    [ Element (ElementId 1) False 5 5
    , Element (ElementId 2) False 4 8
    , Element (ElementId 3) False 2 1
    , Element (ElementId 4) True 3 7
    , Element (ElementId 5) False 4 4
    , Element (ElementId 6) False 5 9
    , Element (ElementId 7) False 9 3
    , Element (ElementId 8) True 8 7
    , Element (ElementId 9) False 1 3
    , Element (ElementId 10) False 7 3
    , Element (ElementId 11) False 1 2
    , Element (ElementId 12) True 8 4
    , Element (ElementId 13) False 3 1
    , Element (ElementId 14) False 4 9
    , Element (ElementId 15) False 6 7
    ]


overFilledKnapsack :: Genome Element
overFilledKnapsack = V.fromList $
    [ Element (ElementId 1) True 5 5
    , Element (ElementId 2) False 4 8
    , Element (ElementId 3) False 2 1
    , Element (ElementId 4) True 3 7
    , Element (ElementId 5) False 4 4
    , Element (ElementId 6) True 5 9
    , Element (ElementId 7) True 9 3
    , Element (ElementId 8) True 8 7
    , Element (ElementId 9) False 1 3
    , Element (ElementId 10) True 7 3
    , Element (ElementId 11) False 1 2
    , Element (ElementId 12) True 8 4
    , Element (ElementId 13) False 3 1
    , Element (ElementId 14) True 4 9
    , Element (ElementId 15) True 6 7
    ]


emptyKnapsack :: Genome Element
emptyKnapsack = V.fromList $
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
