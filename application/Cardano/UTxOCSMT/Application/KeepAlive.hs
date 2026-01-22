{-# LANGUAGE NumericUnderscores #-}

module Cardano.UTxOCSMT.Application.KeepAlive
    ( keepAliveApplication
    ) where

import Cardano.UTxOCSMT.Ouroboros.Types (KeepAliveApplication)
import Control.Concurrent (threadDelay)
import Data.Function (fix)
import Ouroboros.Network.Protocol.KeepAlive.Client
    ( KeepAliveClient (..)
    , KeepAliveClientSt (SendMsgKeepAlive)
    )
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))

keepAliveApplication :: KeepAliveApplication
keepAliveApplication =
    KeepAliveClient
        $ ($ 1)
        $ fix
        $ \go n -> do
            threadDelay 1_000_000
            pure $ SendMsgKeepAlive Cookie{unCookie = n} $ go (n + 1)
