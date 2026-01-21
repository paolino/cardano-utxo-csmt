module Data.Tracer.Intercept
    ( intercept
    )
where

import Control.Tracer (Tracer (Tracer))
import Data.Foldable (for_)

-- | Modify a Tracer to emit to another Tracer based on a partial mapping function
intercept
    :: Monad m => Tracer m b -> (a -> Maybe b) -> Tracer m a -> Tracer m a
intercept (Tracer f) g (Tracer h) = Tracer $ \a -> do
    for_ (g a) f
    h a
