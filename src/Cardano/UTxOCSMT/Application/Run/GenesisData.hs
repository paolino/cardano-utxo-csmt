module Cardano.UTxOCSMT.Application.Run.GenesisData
    ( readGenesisTxOuts
    ) where

import Cardano.Chain.Genesis
    ( GenesisData (..)
    , GenesisDataError
    , GenesisNonAvvmBalances (..)
    )
import Cardano.Chain.Genesis qualified as Byron
import Cardano.Chain.UTxO (TxOut (..))
import Control.Exception (Exception, throwIO)
import Control.Monad.Except (runExceptT)
import Data.Map.Strict qualified as Map

newtype GenesisDataException = GenesisDataException GenesisDataError
    deriving (Show)

instance Exception GenesisDataException

-- | Extract the UTxOs from a byron genesis file
readGenesisTxOuts
    :: FilePath
    -> IO [TxOut]
readGenesisTxOuts fp = do
    r <- runExceptT . Byron.readGenesisData $ fp
    case r of
        Left err -> throwIO $ GenesisDataException err
        Right gd -> pure $ getUTxOs $ fst gd

getUTxOs :: GenesisData -> [TxOut]
getUTxOs = fromNonAvvmBalances . gdNonAvvmBalances

fromNonAvvmBalances :: GenesisNonAvvmBalances -> [TxOut]
fromNonAvvmBalances (GenesisNonAvvmBalances m) =
    uncurry TxOut <$> Map.toList m
