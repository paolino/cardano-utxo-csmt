{-# LANGUAGE NoStrictData #-}

module Data.List.SampleFibonacci
    ( sampleAtFibonacciIntervals
    , atMost
    , fibonacci
    , fibonacciIntervals
    , sampleAll
    ) where

import Data.Function (fix)
import Data.Maybe (maybeToList)

data Sequence a = Cons
    { sequenceHead :: a
    , sequenceTail :: Sequence a
    }

scan :: (a -> b -> a) -> a -> Sequence b -> Sequence a
scan f q (Cons x xs) =
    let q' = f q x
    in  Cons q' (scan f q' xs)

toList :: Sequence a -> [a]
toList (Cons x xs) = x : toList xs

infiniteZip
    :: (a -> b -> c) -> Sequence a -> Sequence b -> Sequence c
infiniteZip f (Cons x xs) (Cons y ys) =
    Cons{sequenceHead = f x y, sequenceTail = infiniteZip f xs ys}

fibonacciSequence :: Sequence Int
fibonacciSequence =
    Cons 0
        $ Cons 1
        $ infiniteZip (+) fibonacciSequence
        $ sequenceTail fibonacciSequence

fibonacci :: [Int]
fibonacci = toList fibonacciSequence

fibonacciSequenceIntervals :: Sequence Int
fibonacciSequenceIntervals = scan (+) 0 fibonacciSequence

fibonacciIntervals :: [Int]
fibonacciIntervals = toList fibonacciSequenceIntervals

{- | Try to get at most n elements from the monadic action
if the action returns Nothing before n is reached, it returns the previously
collect element
-}
atMost :: Monad m => Int -> m (Maybe a) -> m (Maybe a)
atMost n _
    | n <= 0 = pure Nothing
atMost n f = do
    mh <- f
    case mh of
        Nothing -> pure Nothing
        Just l0 -> flip ($ l0) (n - 1) $ fix $ \go l -> \case
            0 -> pure $ Just l
            m -> do
                mh' <- f
                case mh' of
                    Nothing -> pure $ Just l
                    Just y -> go y $ m - 1

{- | Sample elements from the monadic action at positions defined by the Fibonacci sequence
it's implied that the action will eventually return Nothing
it's implied that the action will move forward one element at every call in some sequence
if the sequence is not empty the last element will be always returned even if not at a Fibonacci position
-}
sampleAtFibonacciIntervals
    :: Monad m => m (Maybe a) -> m [a]
sampleAtFibonacciIntervals f = ($ fibonacciSequence) $ fix $ \go (Cons n ns) -> do
    ml <- atMost (n - 1) f
    mx <- f
    case mx of
        Nothing -> pure $ maybeToList ml
        Just x -> (x :) <$> go ns

sampleAll :: Monad m => m (Maybe a) -> m [a]
sampleAll f = fix $ \go -> do
    mx <- f
    case mx of
        Nothing -> pure []
        Just x -> (x :) <$> go
