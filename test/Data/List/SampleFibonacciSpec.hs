module Data.List.SampleFibonacciSpec
    ( spec
    )
where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf)
import Data.List.SampleFibonacci
    ( atMost
    , fibonacci
    , fibonacciIntervals
    , sampleAtFibonacciIntervals
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , NonEmptyList (..)
    , Positive (..)
    , forAll
    , property
    , suchThat
    )

sampleList :: [Int] -> IO (IO (Maybe Int))
sampleList xs = do
    ref <- newIORef xs
    pure $ do
        lst <- readIORef ref
        case lst of
            [] -> pure Nothing
            y : ys -> do
                writeIORef ref ys
                pure $ Just y

spec :: Spec
spec = do
    describe "sampleList" $ do
        it "returns Nothing on empty list" $ do
            sampler <- sampleList []
            result <- sampler
            result `shouldBe` Nothing

        it "returns elements in order" $ do
            sampler <- sampleList [1, 2, 3]
            result1 <- sampler
            result2 <- sampler
            result3 <- sampler
            result4 <- sampler
            result1 `shouldBe` Just 1
            result2 `shouldBe` Just 2
            result3 `shouldBe` Just 3
            result4 `shouldBe` Nothing

    describe "fibonacci" $ do
        it "generates the correct Fibonacci sequence" $ do
            ( [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]
                    `isInfixOf` fibonacci
                )
                `shouldBe` True
    describe "fibonacciIntervals" $ do
        it "generates the correct Fibonacci intervals" $ do
            ( [0, 1, 2, 4, 7, 12, 20, 33, 54, 88, 143, 232, 376]
                    `isInfixOf` fibonacciIntervals
                )
                `shouldBe` True
    describe "atMost" $ do
        it "returns Nothing when n is 0" $ property $ \xs -> do
            sampler <- sampleList xs
            result <- atMost 0 sampler
            result `shouldBe` Nothing

        it "returns Nothing when action returns Nothing immediately"
            $ property
            $ \(Positive n) -> do
                sampler <- sampleList []
                result <- atMost n sampler
                result `shouldBe` Nothing

        it "returns the nth element when n is within available elements"
            $ property
            $ \(NonEmpty xs) -> forAll
                ( arbitrary
                    `suchThat` (<= length xs)
                    `suchThat` (> 0)
                )
                $ \n -> do
                    sampler <- sampleList xs
                    result <- atMost n sampler
                    result `shouldBe` Just (xs !! (n - 1))

        it
            "returns the last element before Nothing when n is larger than available elements"
            $ property
            $ \(NonEmpty xs) -> forAll (arbitrary `suchThat` (> length xs))
                $ \n -> do
                    sampler <- sampleList xs
                    result <- atMost n sampler
                    result `shouldBe` Just (last xs)

    describe "filterAtFibonacci" $ do
        it "samples correctly from an empty list" $ do
            sampler <- sampleList []
            result <- sampleAtFibonacciIntervals sampler
            result `shouldBe` []
        it "samples correctly from a non-empty list" $ do
            let xs = [0 .. 377]
            sampler <- sampleList xs
            result <- sampleAtFibonacciIntervals sampler
            result
                `shouldBe` [0, 1, 2, 4, 7, 12, 20, 33, 54, 88, 143, 232, 376, 377]
