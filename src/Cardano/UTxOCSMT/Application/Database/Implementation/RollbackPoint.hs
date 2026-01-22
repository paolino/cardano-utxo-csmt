module Cardano.UTxOCSMT.Application.Database.Implementation.RollbackPoint
    ( RollbackPoint (..)
    , RollbackPointKV
    , rollbackPointPrism
    , withOriginPrism
    )
where

import Cardano.UTxOCSMT.Application.Database.Interface
    ( Operation (..)
    )
import Control.Lens (Prism', preview, prism', review)
import Control.Monad (forM_, replicateM)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Serialize
    ( Get
    , PutM
    , getByteString
    , getWord32be
    , getWord8
    , putByteString
    , putWord32be
    , putWord8
    )
import Data.Serialize.Extra (evalGetM, evalPutM)
import Database.KV.Transaction (KV)
import Ouroboros.Network.Point (WithOrigin (..))

-- | Represents a rollback point in the database
data RollbackPoint slot hash key value = RollbackPoint
    { rbpHash :: hash
    , rbpInverseOperations :: [Operation key value]
    , rpbMerkleRoot :: Maybe hash
    }

-- | Type alias for the KV column storing rollback points
type RollbackPointKV slot hash key value =
    KV (WithOrigin slot) (RollbackPoint slot hash key value)

putReview :: Prism' ByteString a -> a -> PutM ()
putReview p x = do
    let y = review p x
        l = B.length y
    putWord32be $ fromIntegral l
    putByteString y

getPreview :: Prism' ByteString a -> Get a
getPreview p = do
    bs <- getWord32be >>= getByteString . fromIntegral
    case preview p bs of
        Just x -> pure x
        Nothing -> fail "getPreview: prism decoding failed"

-- | Prism for serializing/deserializing RollbackPoint
rollbackPointPrism
    :: forall slot hash key value
     . Prism' ByteString hash
    -> Prism' ByteString key
    -> Prism' ByteString value
    -> Prism' ByteString (RollbackPoint slot hash key value)
rollbackPointPrism hashPrism keyPrism valuePrism =
    prism' encode decode
  where
    encode :: RollbackPoint slot hash key value -> ByteString
    encode RollbackPoint{rbpHash, rbpInverseOperations, rpbMerkleRoot} =
        evalPutM $ do
            putReview hashPrism rbpHash
            let opsCount = fromIntegral (length rbpInverseOperations)
            putWord32be opsCount
            forM_ rbpInverseOperations $ \case
                Delete k -> do
                    putWord8 0
                    putReview keyPrism k
                Insert k v -> do
                    putWord8 1
                    putReview keyPrism k
                    putReview valuePrism v
            case rpbMerkleRoot of
                Nothing -> putWord8 0
                Just h -> do
                    putWord8 1
                    putReview hashPrism h

    decode :: ByteString -> Maybe (RollbackPoint slot hash key value)
    decode = evalGetM $ do
        rbpHash <- getPreview hashPrism
        opsCount <- getWord32be
        rbpInverseOperations <- replicateM (fromIntegral opsCount) $ do
            tag <- getWord8
            case tag of
                0 -> do
                    k <- getPreview keyPrism
                    pure $ Delete k
                1 -> do
                    k <- getPreview keyPrism
                    v <- getPreview valuePrism
                    pure $ Insert k v
                _ -> fail "mkRollbackPointCodecs: invalid operation tag"
        mbTag <- getWord8
        rpbMerkleRoot <- case mbTag of
            0 -> pure Nothing
            1 -> Just <$> getPreview hashPrism
            _ -> fail "mkRollbackPointCodecs: invalid merkle root tag"
        pure $ RollbackPoint{rbpHash, rbpInverseOperations, rpbMerkleRoot}

withOriginPrism
    :: forall slot
     . Prism' ByteString slot
    -> Prism' ByteString (WithOrigin slot)
withOriginPrism slotP = prism' encode decode
  where
    encode :: WithOrigin slot -> ByteString
    encode Origin = B.singleton 0
    encode (At slot) =
        B.cons 1 $ evalPutM $ putReview slotP slot

    decode :: ByteString -> Maybe (WithOrigin slot)
    decode = evalGetM getOrigin

    getOrigin :: Get (WithOrigin slot)
    getOrigin = do
        tag <- getWord8
        case tag of
            0 -> pure Origin
            1 -> At <$> getPreview slotP
            _ -> fail "withOriginPrism: invalid WithOrigin tag"
