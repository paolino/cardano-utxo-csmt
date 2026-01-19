module Cardano.N2N.Client.Application.Database.RocksDBSpec
    ( spec
    )
where

import CSMT.Backend.RocksDB (RunRocksDB (..))
import CSMT.Hashes (Hash, fromKVHashes, hashHashing, isoHash, mkHash)
import Cardano.N2N.Client.Application.Database.Implementation
    ( Point (..)
    , mkUpdate
    )
import Cardano.N2N.Client.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (ArmageddonParams)
    , setup
    )
import Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Prisms (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Query
    ( mkTransactionedQuery
    )
import Cardano.N2N.Client.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    )
import Cardano.N2N.Client.Application.Database.Implementation.Update
    ( PartialHistory (..)
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
import Cardano.N2N.Client.Application.Database.RocksDB
    ( mkRunRocksDBCSMTTransaction
    , mkRunRocksDBTransaction
    )
import Cardano.Slotting.Slot (WithOrigin (..))
import Control.Lens (prism')
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import Data.Char (isAlpha, isAscii)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Serialize (getWord64be, putWord64be)
import Data.Serialize.Extra (evalGetM, evalPutM)
import Database.RocksDB (Config (..), withDBCF)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck
    ( Arbitrary (..)
    , NonNegative (NonNegative)
    , Property
    , Testable (..)
    , ioProperty
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

newtype GenKey = GenKey ByteString
    deriving (Show, Eq, Ord)

padto32 :: ByteString -> ByteString
padto32 bs = bs `BC.append` BC.replicate (32 - BC.length bs) '_'

instance Arbitrary GenKey where
    arbitrary =
        GenKey . padto32 . BC.pack
            <$> listOf (arbitrary `suchThat` isAsciiAlpha)
      where
        isAsciiAlpha c = isAscii c && isAlpha c

newtype GenValue = GenValue ByteString
    deriving (Show, Eq, Ord)

instance Arbitrary GenValue where
    arbitrary = GenValue . BC.pack <$> listOf (arbitrary `suchThat` isAsciiAlpha)
      where
        isAsciiAlpha c = isAscii c && isAlpha c

newtype GenHash = GenHash Hash
    deriving (Show, Eq, Ord)

instance Arbitrary GenHash where
    arbitrary =
        GenHash . mkHash . BC.pack
            <$> listOf (arbitrary `suchThat` isAsciiAlpha)
      where
        isAsciiAlpha c = isAscii c && isAlpha c

csmtContext :: CSMTContext Hash ByteString ByteString
csmtContext = CSMTContext{fromKV = fromKVHashes, hashing = hashHashing}

prisms :: Prisms Int Hash ByteString ByteString
prisms =
    Prisms
        { slotP =
            prism'
                (evalPutM . putWord64be . fromIntegral)
                (fmap fromIntegral . evalGetM getWord64be)
        , hashP = isoHash
        , keyP = id
        , valueP = id
        }

runRocksDBProperties
    :: WithExpected IO (Point Int Hash) ByteString ByteString a
    -> IO a
runRocksDBProperties prop =
    withSystemTempDirectory "rocksdb-test" $ \dir ->
        withRocksDB dir $ \(RunRocksDB r) -> do
            db <- r ask
            setup (txRunner db) armageddonParams
            runWithExpected (context db) (update db) prop
  where
    txRunner db = mkRunRocksDBCSMTTransaction db prisms csmtContext
    armageddonParams = ArmageddonParams 1000 (mkHash "")
    context db =
        Context
            { contextDatabase = query db
            , contextGenerator =
                Generator
                    { genSlot = do
                        GenSlot x <- arbitrary

                        GenHash h <- arbitrary
                        pure $ Point x h
                    , genKey = do
                        GenKey x <- arbitrary
                        pure x
                    , genValue = do
                        GenValue v <- arbitrary
                        pure v
                    }
            }

    query db = mkTransactionedQuery $ mkRunRocksDBTransaction db prisms
    update db = mkUpdate Complete armageddonParams $ txRunner db

test
    :: PropertyWithExpected
        IO
        (Point Int Hash)
        ByteString
        ByteString
        ()
    -> Property
test f =
    property
        $ fmap (ioProperty . runRocksDBProperties)
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
    describe "RocksDB Database" $ do
        it "can be opened" $ test $ pure ()
        it "can be queried when empty" $ test $ pure ()
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
            logOnFailure "Starting forward after tip test"
            getDump >>= logOnFailure . ("Initial dump: " ++) . show
            _ <- populateWithSomeContent
            getDump >>= logOnFailure . ("After load dump: " ++) . show
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

withRocksDB
    :: FilePath
    -> (RunRocksDB -> IO b)
    -> IO b
withRocksDB path action = do
    withDBCF
        path
        config
        [ ("kv", config)
        , ("csmt", config)
        , ("rollbacks", config)
        ]
        $ \db -> do
            action $ RunRocksDB $ flip runReaderT db

config :: Config
config =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Just 1
        , prefixLength = Nothing
        , bloomFilter = False
        }
