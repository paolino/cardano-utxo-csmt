module Cardano.N2N.Client.Application.Database.Implementation.Columns
    ( Columns (..)
    , Prisms (..)
    , codecs
    )
where

import CSMT.Interface (Indirect, Key, csmtCodecs)
import Cardano.N2N.Client.Application.Database.Implementation.RollbackPoint
    ( RollbackPointKV
    , rollbackPointPrism
    , withOriginPrism
    )
import Control.Lens (Prism', type (:~:) (Refl))
import Data.ByteString (ByteString)
import Database.KV.Transaction
    ( Codecs (..)
    , DMap
    , DSum ((:=>))
    , GCompare (..)
    , GEq (..)
    , GOrdering (..)
    , KV
    , mkCols
    )

-- | Structure of the database used by this application
data Columns slot hash key value x where
    KVCol :: Columns slot hash key value (KV key value)
        -- ^ Key-Value column for utxos
    CSMTCol :: Columns slot hash key value (KV Key (Indirect hash))
        -- ^ CSMT column for storing the CSMT of the UTxO set
    RollbackPoints
        :: Columns slot hash key value (RollbackPointKV slot hash key value)
        -- ^ Column for storing rollback points

instance GEq (Columns slot hash key value) where
    geq KVCol KVCol = Just Refl
    geq CSMTCol CSMTCol = Just Refl
    geq RollbackPoints RollbackPoints = Just Refl
    geq _ _ = Nothing

instance GCompare (Columns slot hash key value) where
    gcompare KVCol KVCol = GEQ
    gcompare KVCol _ = GLT
    gcompare _ KVCol = GGT
    gcompare CSMTCol CSMTCol = GEQ
    gcompare CSMTCol RollbackPoints = GLT
    gcompare RollbackPoints CSMTCol = GGT
    gcompare RollbackPoints RollbackPoints = GEQ

-- | Prisms for serializing/deserializing keys and values
data Prisms slot hash key value = Prisms
    { slotP :: Prism' ByteString slot
    , hashP :: Prism' ByteString hash
    , keyP :: Prism' ByteString key
    , valueP :: Prism' ByteString value
    }

-- | Codecs for the database columns
codecs
    :: Prisms slot hash key value
    -> DMap (Columns slot hash key value) Codecs
codecs Prisms{keyP, hashP, slotP, valueP} =
    mkCols
        [ KVCol :=> Codecs{keyCodec = keyP, valueCodec = valueP}
        , CSMTCol :=> csmtCodecs hashP
        , RollbackPoints
            :=> Codecs
                { keyCodec = withOriginPrism slotP
                , valueCodec = rollbackPointPrism hashP keyP valueP
                }
        ]
