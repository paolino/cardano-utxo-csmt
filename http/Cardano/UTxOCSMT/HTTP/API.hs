module Cardano.UTxOCSMT.HTTP.API
    ( api
    )
where

import Cardano.UTxOCSMT.Application.Metrics (Metrics)
import Servant (Get, JSON, Proxy (..), type (:>))

type API = "metrics" :> Get '[JSON] Metrics

api :: Proxy API
api = Proxy