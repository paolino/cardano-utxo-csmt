module Data.Tracer.TraceWith (pattern TraceWith, tracer, trace, contra)
where

import Control.Arrow ((&&&))
import Control.Tracer (Tracer (..))

-- without this passage GHC cannot expose a pattern synonym with a rank-n type
newtype Contra m a = Contra (forall b. (b -> a) -> Tracer m b)

mkContra :: Tracer m a -> Contra m a
mkContra (Tracer f) = Contra (\g -> Tracer (f . g))

{-# COMPLETE TraceWith #-}

-- | A pattern synonym to deconstruct a 'Tracer' into itself, its underlying emitter function,
-- and a contravariant mapping function against itself
pattern TraceWith
    :: Tracer m a
    -> (a -> m ())
    -> (forall b. (b -> a) -> Tracer m b)
    -> Tracer m a
pattern TraceWith{tracer, trace, contra} <-
    tracer@(id &&& mkContra -> (Tracer trace, Contra contra))
