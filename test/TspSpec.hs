module TspSpec where

import Tsp
import Genetic
import Test.Hspec
import qualified Data.Vector as V

spec :: Spec
spec = do
    describe "adjacents" $ do
        let cs1 = fmap City [1,2,3,4,5,6,7,8,9]
        let cs2 = fmap City [4,1,2,8,7,6,9,3,5]
        it "gives back the adjacents cities for two sets" $ do
            adjacents (City 1) cs1 cs2 `shouldBe` [City 2, City 9, City 4]
            adjacents (City 2) cs1 cs2 `shouldBe` [City 1, City 3, City 8]
            adjacents (City 5) cs1 cs2 `shouldBe` [City 4, City 6, City 3]
            adjacents (City 7) cs1 cs2 `shouldBe` [City 6, City 8]
