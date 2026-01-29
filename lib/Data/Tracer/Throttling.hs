module Data.Tracer.Throttling
    ( every
    ) where

import Control.Tracer (Tracer (..), traceWith)

every :: Monad m => Int -> Tracer m a -> Tracer m a
every n tracer = go 0
  where
    go count = Tracer $ \a -> do
        let count' = count + 1
        if count' >= n
            then do
                traceWith tracer a
                runTracer (go 0) a
            else
                runTracer (go count') a
