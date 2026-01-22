module Main (main)
where

import Cardano.UTxOCSMT.HTTP.Swagger (renderSwaggerJSON)
import Data.ByteString.Lazy.Char8 qualified as BL

main :: IO ()
main = BL.putStrLn renderSwaggerJSON