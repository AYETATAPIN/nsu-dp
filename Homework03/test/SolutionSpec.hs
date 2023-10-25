{-# OPTIONS_GHC -Wno-type-defaults #-}
module SolutionSpec
    ( spec
    ) where


import Data.List ( nub )

import Test.Hspec ( describe, Spec, it, shouldBe )
import Test.Hspec.QuickCheck ( modifyMaxSuccess )
import Test.QuickCheck ( property )

import Solution ( pythagoreanTriples, primitivePythagoreanTriples, unique, perfectNumbers, cantorPairs, minimalDistance )

spec :: Spec
spec = do
    describe "unique" $ do
        modifyMaxSuccess (const 10000) $ it "is equal to (\\x -> length (nub x) == length x)" $ property $
            ((\x -> unique x `shouldBe` (length (nub x) == length x)) :: ([Int] -> IO ()))

    describe "pythagoreanTriples" $ do
        it "produces Pythagorean triples" $ do
            take 10 pythagoreanTriples `shouldBe` [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20),(15,20,25),(7,24,25),(10,24,26),(20,21,29)]

    describe "primitivePythagoreanTriples" $ do
        it "produces primitive Pythagorean triples" $ do
            take 10 primitivePythagoreanTriples `shouldBe` [(3,4,5),(5,12,13),(8,15,17),(7,24,25),(20,21,29),(12,35,37),(9,40,41),(28,45,53),(11,60,61),(33,56,65)]

    describe "perfectNumbers" $ do
        it "can produce first 4 perfect numbers" $ do
            take 4 perfectNumbers `shouldBe` [6,28,496,8128]

    describe "cantorPairs" $ do
        it "produces a list, which elements are Cantor pairs" $ do
            take 10 cantorPairs `shouldBe` [(0,0),(1,0),(0,1),(2,0),(1,1),(0,2),(3,0),(2,1),(1,2),(0,3)]

        it "and does this effectively" $ do
            cantorPairs !! 10000000 `shouldBe` (1627,2844)

    describe "minimalDistance" $ do
        it "finds the minimal euclidian distance between all possible combinations of two points from a given list of points" $ do
            (abs (minimalDistance [(-5.23,4.2),(2.456,13.234),(11241,211.2),(-1,0)] - 5.9609) < 0.001) `shouldBe` True
