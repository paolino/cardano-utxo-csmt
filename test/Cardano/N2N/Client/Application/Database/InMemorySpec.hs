module Cardano.N2N.Client.Application.Database.InMemorySpec
    ( spec
    )
where

import Cardano.N2N.Client.Application.Database.InMemory
    ( InMemoryState
    , mkInMemoryDatabaseSimple
    , runInMemoryState
    , updateInMemory
    )
import Cardano.N2N.Client.Application.Database.Properties
    ( findValue
    , logOnFailure
    , populateWithSomeContent
    , propertyForwardAfterTipAppliesChanges
    , propertyForwardBeforeTipIsNoOp
    , propertyForwardFinalityAfterFinalityReduceTheRollbackWindow
    , propertyRollbackAfterBeforeTipUndoesChanges
    , propertyRollbackAfterTipDoesNothing
    , propertyRollbackBeforeFinalityTruncatesTheDatabase
    , propertyTipIsAfterFinalityOrMissing
    )
import Cardano.N2N.Client.Application.Database.Properties.Expected
    ( Context (..)
    , Generator (..)
    , PropertyWithExpected
    , WithExpected
    , getDump
    , runWithExpected
    )
import Cardano.Slotting.Slot (WithOrigin (..))
import Data.Char (isAlpha, isAscii)
import Data.Functor.Identity (Identity (..))
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (NonNegative)
    , Positive (..)
    , Property
    , Testable (..)
    , listOf
    , scale
    , suchThat
    )
import Test.QuickCheck.Monadic (monadic')

newtype GenSlot = GenSlot Int
    deriving (Show, Eq, Ord)

instance Arbitrary GenSlot where
    arbitrary = do
        NonNegative x <- arbitrary
        pure $ GenSlot x

newtype GenKey = GenKey Int
    deriving (Show, Eq, Ord)

instance Arbitrary GenKey where
    arbitrary = do
        Positive x <- arbitrary
        pure $ GenKey x

newtype GenValue = GenValue String
    deriving (Show, Eq, Ord)
instance Arbitrary GenValue where
    arbitrary = GenValue <$> listOf (arbitrary `suchThat` isAsciiAlpha)
      where
        isAsciiAlpha c = isAscii c && isAlpha c
runInMemoryProperties
    :: WithExpected (InMemoryState Identity Int Int String) Int Int String a
    -> a
runInMemoryProperties prop =
    runIdentity
        $ runInMemoryState
        $ runWithExpected context box prop
  where
    context =
        Context
            { contextDatabase = mkInMemoryDatabaseSimple
            , contextGenerator =
                Generator
                    { genSlot = do
                        GenSlot x <- arbitrary
                        pure x
                    , genKey = do
                        GenKey x <- arbitrary
                        pure x
                    , genValue = do
                        GenValue v <- arbitrary
                        pure v
                    }
            }
    box = updateInMemory mkInMemoryDatabaseSimple

test
    :: PropertyWithExpected
        (InMemoryState Identity Int Int String)
        Int
        Int
        String
        ()
    -> Property
test f =
    property
        $ fmap runInMemoryProperties
        $ scale (`div` 3)
        $ monadic' f

spec :: Spec
spec = do
    describe "Slot" $ do
        it "At 0 > Origin" $ do
            (At (0 :: Int) > Origin) `shouldBe` True
    describe "findValue" $ do
        it "finds existing value" $ do
            let lst = [(1 :: Int, "one" :: String), (2, "two"), (3, "three")]
            let result = findValue 2 Nothing lst
            result `shouldBe` Just "two"
        it
            "returns the latest value if the requested key is beyond the largest key"
            $ do
                let lst = [(1 :: Int, "one" :: String), (2, "two"), (3, "three")]
                let result = findValue 4 Nothing lst
                result `shouldBe` Just "three"
        it "returns Nothing if the requested key is before the smallest key" $ do
            let lst = [(1 :: Int, "one" :: String), (2, "two"), (3, "three")]
            let result = findValue 0 Nothing lst
            result `shouldBe` Nothing
        it
            "finds a previous value if the requested key is missing but less than the next key"
            $ do
                let lst = [(1 :: Int, "one" :: String), (3, "three")]
                let result = findValue 2 Nothing lst
                result `shouldBe` Just "one"
    describe "InMemory Database" $ do
        it "can be populated" $ test $ do
            _ <- populateWithSomeContent
            pure ()
        it "keeps tip after finality" $ test $ do
            _ <- populateWithSomeContent
            propertyTipIsAfterFinalityOrMissing
        it "will not forward before tip" $ test $ do
            _ <- populateWithSomeContent
            propertyForwardBeforeTipIsNoOp
        it "will forward after tip and apply changes" $ test $ do
            _ <- populateWithSomeContent
            propertyForwardAfterTipAppliesChanges
        it "will not rollback to after tip" $ test $ do
            _ <- populateWithSomeContent
            propertyRollbackAfterTipDoesNothing
        it "will rollback before tip and after finality" $ test $ do
            d0 <- getDump
            ds <- populateWithSomeContent
            logOnFailure $ "Generated dumps: " ++ show ds
            let past = (Origin, d0) :| ds
            propertyRollbackAfterBeforeTipUndoesChanges past
        it "will truncate when rolling back before finality" $ test $ do
            _ <- populateWithSomeContent
            propertyRollbackBeforeFinalityTruncatesTheDatabase
        it "will forward finality" $ test $ do
            d0 <- getDump
            ds <- populateWithSomeContent
            logOnFailure $ "Generated dumps: " ++ show ds
            let past = (Origin, d0) :| ds
            propertyForwardFinalityAfterFinalityReduceTheRollbackWindow past
