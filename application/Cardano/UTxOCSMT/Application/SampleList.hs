module Cardano.UTxOCSMT.Application.SampleList
    ( sampleList
    )
where

import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.List.NonEmpty qualified as NE

fib :: NonEmpty Int
fib = 0 `NE.cons` (1 :| zipWith (+) (toList fib) (NE.tail fib))

-- | sample elements from a list at fibonacci intervals
sampleList :: [a] -> [a]
sampleList = go fib
  where
    go :: NonEmpty Int -> [a] -> [a]
    go _ [] = []
    go (n :| ns) ys =
        case drop (n - 1) ys of
            [] -> []
            (z : zs) -> z : go (NE.fromList ns) zs
