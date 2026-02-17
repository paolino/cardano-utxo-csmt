module Cardano.UTxOCSMT.Application.Database.RocksDBSpec
    ( spec
    )
where

import CSMT.Backend.RocksDB (RunRocksDB (..))
import CSMT (FromKV (..))
import CSMT.Hashes
    ( Hash
    , fromKVHashes
    , hashHashing
    , isoHash
    , mkHash
    )
import Cardano.Slotting.Slot (WithOrigin (..))
import Cardano.UTxOCSMT.Application.Database.Implementation.AppConfig
    ( AppConfig (..)
    , decodeAppConfig
    , encodeAppConfig
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Armageddon
    ( ArmageddonParams (ArmageddonParams)
    , setup
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Columns
    ( Prisms (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Query
    ( clearBootstrapInProgress
    , clearSkipSlot
    , getBaseCheckpoint
    , getSkipSlot
    , isBootstrapInProgress
    , mkTransactionedQuery
    , putBaseCheckpoint
    , setBootstrapInProgress
    , setSkipSlot
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Transaction
    ( CSMTContext (..)
    , RunCSMTTransaction (..)
    )
import Cardano.UTxOCSMT.Application.Database.Implementation.Update
    ( mkUpdate
    )
import Cardano.UTxOCSMT.Application.Database.Interface (TipOf)
import Cardano.UTxOCSMT.Application.Database.Properties
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
import Cardano.UTxOCSMT.Application.Database.Properties.Expected
    ( Context (..)
    , Generator (..)
    , PropertyWithExpected
    , WithExpected
    , getDump
    , runWithExpected
    )
import Cardano.UTxOCSMT.Application.Database.RocksDB
    ( newRunRocksDBCSMTTransaction
    , newRunRocksDBTransaction
    )
import Control.Lens (preview, prism', review)
import Control.Monad.Trans.Reader (ReaderT (..), ask)
import Control.Tracer (nullTracer)
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
    , Gen
    , NonNegative (NonNegative)
    , Property
    , Testable (..)
    , forAll
    , ioProperty
    , listOf
    , scale
    , suchThat
    )
import Test.QuickCheck.Monadic (monadic')

-- | For testing, TipOf Int = Int (slot equals tip type)
type instance TipOf Int = Int

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

-- | Generator for AppConfig Int
genAppConfig :: Gen (AppConfig Int)
genAppConfig =
    AppConfig
        <$> arbitrary
        <*> arbitrary
        <*> arbitrary

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

-- | Encode slot for config storage (uses the prism)
encodeSlot :: Int -> ByteString
encodeSlot = review (slotP prisms)

-- | Decode slot from config storage (uses the prism)
decodeSlot :: ByteString -> Maybe Int
decodeSlot = preview (slotP prisms)

runRocksDBProperties
    :: WithExpected IO Int ByteString ByteString a
    -> IO a
runRocksDBProperties prop =
    withSystemTempDirectory "rocksdb-test" $ \dir ->
        withRocksDB dir $ \(RunRocksDB r) -> do
            db <- r ask
            runner <- txRunner db
            contextDatabase <- query db
            setup nullTracer runner armageddonParams
            let context =
                    Context
                        { contextDatabase
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
            runWithExpected context (update runner) prop
  where
    txRunner db = newRunRocksDBCSMTTransaction db prisms csmtContext
    armageddonParams = ArmageddonParams 1000 (mkHash "")
    query db =
        mkTransactionedQuery (isoK $ fromKV csmtContext)
            <$> newRunRocksDBTransaction db prisms
    update =
        mkUpdate
            nullTracer
            (const $ mkHash "")
            (\_ _ -> pure ())
            armageddonParams

test
    :: PropertyWithExpected
        IO
        Int
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
    describe "AppConfig CBOR" $ do
        it "roundtrips through encode/decode"
            $ forAll genAppConfig
            $ \cfg ->
                decodeAppConfig decodeSlot (encodeAppConfig encodeSlot cfg)
                    == Just cfg
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
    describe "ConfigCol operations" $ do
        it "stores and retrieves base checkpoint" $ do
            withSystemTempDirectory "rocksdb-config-test" $ \dir ->
                withRocksDB dir $ \(RunRocksDB r) -> do
                    db <- r ask
                    RunCSMTTransaction{txRunTransaction} <-
                        newRunRocksDBCSMTTransaction db prisms csmtContext
                    -- Initially no checkpoint
                    result1 <- txRunTransaction $ getBaseCheckpoint decodeSlot
                    result1 `shouldBe` Nothing
                    -- Store checkpoint
                    txRunTransaction
                        $ putBaseCheckpoint decodeSlot encodeSlot (42 :: Int)
                    -- Retrieve checkpoint
                    result2 <- txRunTransaction $ getBaseCheckpoint decodeSlot
                    result2 `shouldBe` Just 42
        it "tracks bootstrap in progress marker" $ do
            withSystemTempDirectory "rocksdb-bootstrap-test" $ \dir ->
                withRocksDB dir $ \(RunRocksDB r) -> do
                    db <- r ask
                    RunCSMTTransaction{txRunTransaction} <-
                        newRunRocksDBCSMTTransaction db prisms csmtContext
                    -- Initially not in progress
                    inProgress1 <-
                        txRunTransaction
                            $ isBootstrapInProgress decodeSlot
                    inProgress1 `shouldBe` False
                    -- Set marker
                    txRunTransaction
                        $ setBootstrapInProgress decodeSlot encodeSlot
                    -- Now in progress
                    inProgress2 <-
                        txRunTransaction
                            $ isBootstrapInProgress decodeSlot
                    inProgress2 `shouldBe` True
                    -- Clear marker
                    txRunTransaction
                        $ clearBootstrapInProgress decodeSlot encodeSlot
                    -- No longer in progress
                    inProgress3 <-
                        txRunTransaction
                            $ isBootstrapInProgress decodeSlot
                    inProgress3 `shouldBe` False
        it "stores and retrieves skip slot" $ do
            withSystemTempDirectory "rocksdb-skip-test" $ \dir ->
                withRocksDB dir $ \(RunRocksDB r) -> do
                    db <- r ask
                    RunCSMTTransaction{txRunTransaction} <-
                        newRunRocksDBCSMTTransaction db prisms csmtContext
                    -- Initially no skip slot
                    slot1 <- txRunTransaction $ getSkipSlot decodeSlot
                    slot1 `shouldBe` Nothing
                    -- Set skip slot
                    txRunTransaction
                        $ setSkipSlot decodeSlot encodeSlot 12345678
                    -- Retrieve skip slot
                    slot2 <- txRunTransaction $ getSkipSlot decodeSlot
                    slot2 `shouldBe` Just 12345678
                    -- Clear skip slot
                    txRunTransaction
                        $ clearSkipSlot decodeSlot encodeSlot
                    -- No longer set
                    slot3 <- txRunTransaction $ getSkipSlot decodeSlot
                    slot3 `shouldBe` Nothing
        it "survives database close and reopen (crash recovery)" $ do
            let testDir = "/tmp/smoke-test-db"
            -- First session: set skip slot then "crash" (close DB)
            withRocksDB testDir $ \(RunRocksDB r) -> do
                db <- r ask
                RunCSMTTransaction{txRunTransaction} <-
                    newRunRocksDBCSMTTransaction db prisms csmtContext
                txRunTransaction
                    $ setSkipSlot decodeSlot encodeSlot 98765432
            -- Second session: "restart" and verify skip slot persisted
            withRocksDB testDir $ \(RunRocksDB r) -> do
                db <- r ask
                RunCSMTTransaction{txRunTransaction} <-
                    newRunRocksDBCSMTTransaction db prisms csmtContext
                slot <- txRunTransaction $ getSkipSlot decodeSlot
                slot `shouldBe` Just 98765432

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
        , ("config", config)
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
