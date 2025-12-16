{-# LANGUAGE ConstraintKinds #-}

module Cardano.N2N.Client.Application.Database.Properties
    ( propertyTipIsAfterFinalityOrMissing
    , propertyTipIsTheNewestSlot
    , propertyForwardBeforeTipIsNoOp
    , propertyForwardAfterTipAppliesChanges
    , propertyRollbackAfterTipDoesNothing
    , propertyRollbackAfterBeforeTipUndoesChanges
    , propertyForwardFinalityBeforeFinalityIsNoOp
    , populateWithSomeContent
    , propertyForwardFinalityAfterFinalityReduceTheRollbackWindow
    , propertyForwardFinalityBeyondTipSetsFinalityAsTip
    , propertyRollbackFinalityTruncatesTheDatabase
    )
where

-- ( propertyInitialTipIsNull
-- , propertyRollbackAfterTipDoesNothing
-- , propertyRollbackToSlotBeforeTipTruncatesSlotsAfterIt
-- , Generator (..)
-- , Context
-- , propertyTruncateAfterTipTruncatesSetsFinalityAsTip
-- , propertyTruncateAtSlotBeforeTheTipMovesFinality
-- , propertyRollbackBeforeFinalityTruncatesTheDatabase
-- , propertyTruncateBeforeFinalitySlotIsNoOp
-- , propertyForwardAfterTipOperatesAsExpected
-- , propertyForwardBeforeTipIsNoOp
-- )

import Cardano.N2N.Client.Application.Database.Interface
    ( Dump (..)
    , Operation (..)
    )
import Cardano.N2N.Client.Application.Database.Properties.Expected
    ( Generator (..)
    , PropertyConstraints
    , PropertyWithExpected
    , asksGenerator
    , expectedKeys
    , expectedNewestSlot
    , forwardFinality
    , forwardTip
    , getDump
    , getFinality
    , getTip
    , rollbackFinality
    , rollbackTip
    )
import Control.Monad (replicateM_)
import Control.Monad.State
    ( MonadTrans (..)
    , gets
    )
import Data.List.NonEmpty (NonEmpty (..), toList)
import Ouroboros.Network.Point (WithOrigin (..))
import Test.QuickCheck
    ( Arbitrary (arbitrary)
    , Gen
    , NonNegative (..)
    , Positive (Positive)
    , elements
    , suchThat
    )
import Test.QuickCheck.Gen (frequency)
import Test.QuickCheck.Monadic
    ( PropertyM
    , assertWith
    , pick
    )
import Prelude hiding (truncate)

findValue
    :: Ord key
    => key
    -> Maybe value
    -> [(key, value)]
    -> Maybe value
findValue _key candidate [] = candidate
findValue key candidate ((k, v) : rest)
    | key >= k = findValue key (Just v) rest
    | otherwise = candidate

assert :: Monad m => String -> Bool -> PropertyM m ()
assert = flip assertWith

assertingJust
    :: Monad m
    => String
    -> Maybe a
    -> (a -> PropertyM m ())
    -> PropertyM m ()
assertingJust msg Nothing _ = assert msg False
assertingJust _ (Just x) f = f x

genWithOrigin
    :: Gen slot
    -> Gen (WithOrigin slot)
genWithOrigin genSlot =
    frequency
        [ (1, return Origin)
        , (8, fmap At genSlot)
        ]

propertyTipIsAfterFinalityOrMissing
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyTipIsAfterFinalityOrMissing = do
    tip <- getTip
    finality <- getFinality
    assert
        "Tip slot should be after or equal to finality slot"
        $ tip >= finality

propertyTipIsTheNewestSlot
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyTipIsTheNewestSlot = do
    newestSlot <- lift $ gets expectedNewestSlot
    tip <- getTip
    assert
        "Tip should be equal to the newest slot in the expected contents"
        $ tip == newestSlot

genSlotAfter
    :: PropertyConstraints m slot key value
    => WithOrigin slot
    -> PropertyWithExpected m slot key value slot
genSlotAfter base = do
    Generator{genSlot} <- asksGenerator
    pick $ genSlot `suchThat` (\s -> At s > base)

generateOperationsAfter
    :: PropertyConstraints m slot key value
    => WithOrigin slot
    -> PropertyWithExpected
        m
        slot
        key
        value
        (slot, [Operation key value])
generateOperationsAfter base = do
    Generator{genKey, genValue} <- asksGenerator
    slot <- genSlotAfter base
    let go (0 :: Int) _ = pure []
        go i available = do
            let deleting = do
                    key <- elements available
                    rest <- go (i - 1) (filter (/= key) available)
                    pure (Delete key : rest)
                inserting = do
                    key <- genKey `suchThat` (`notElem` available)
                    value <- genValue
                    rest <- go (i - 1) (key : available)
                    pure (Insert key value : rest)
            frequency
                $ [ (1, deleting) | not (null available)
                  ]
                    <> [ (3, inserting)
                       ]
    availableKeys <- lift $ gets expectedKeys
    Positive n <- pick arbitrary
    ops <- pick $ go n availableKeys
    pure (slot, ops)

-- | Populate the database with some more content. This should be freely interleaved
--   with properties.
populateWithSomeContent
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
populateWithSomeContent = do
    NonNegative n <- pick arbitrary
    replicateM_ n $ do
        tip <- getTip
        (slot, ops) <- generateOperationsAfter tip
        forwardTip slot ops

-- | Property: forwarding at or before tip is a no-op
-- forwarding must move the tip ahead
propertyForwardBeforeTipIsNoOp
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyForwardBeforeTipIsNoOp = do
    Generator{genSlot} <- asksGenerator
    tipBefore <- getTip
    slotToInsertAt <- case tipBefore of
        Origin -> pick genSlot
        At tipSlot -> pick $ genSlot `suchThat` (<= tipSlot)
    (slot, ops) <- generateOperationsAfter (At slotToInsertAt)
    oldDump <- getDump
    forwardTip slot ops
    newDump <- getDump
    assert
        "Forwarding at or before tip should be no-op"
        $ newDump == oldDump

-- | Property: forwarding after tip applies the changes correctly
-- finality stays where it is
-- tip moves to the new slot
-- associations are updated by applying the changes
-- old associations that were not deleted remain
propertyForwardAfterTipAppliesChanges
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyForwardAfterTipAppliesChanges = do
    Generator{genSlot} <- asksGenerator
    tipBefore <- getTip
    slotToInsertAt <- case tipBefore of
        Origin -> pick genSlot
        At tipSlot -> pick $ genSlot `suchThat` (> tipSlot)
    (slot, ops) <- generateOperationsAfter (At slotToInsertAt)
    Dump
        { dumpFinality = oldFinality
        , dumpAssocs = oldContents
        } <-
        getDump
    forwardTip slot ops
    Dump
        { dumpFinality = newFinality
        , dumpTip = newTip
        , dumpAssocs = newContents
        } <-
        getDump

    assert
        "Finality slot should not change after forwarding"
        $ newFinality == oldFinality
    assert
        "Tip should be updated to the newest slot after forwarding"
        $ newTip == At slot
    assert
        "Changes were applied correctly after forwarding"
        $ flip all ops
        $ \case
            Delete key ->
                key `notElem` fmap fst newContents
            Insert key value ->
                (key, value) `elem` newContents
    assert
        "Old contents should be preserved after forwarding"
        $ flip all oldContents
        $ \(k, v) ->
            (k, v) `elem` newContents
                || k `elem` [key | Delete key <- ops]

-- | Property: rolling back at or after tip does nothing
-- rollback to a slot after or at tip should preserve the database as is
propertyRollbackAfterTipDoesNothing
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyRollbackAfterTipDoesNothing = do
    Generator{genSlot} <- asksGenerator
    tipBefore <- getTip
    slotToRollbackTo <- case tipBefore of
        Origin -> pick $ genWithOrigin genSlot
        At tipSlot -> pick $ At <$> genSlot `suchThat` (>= tipSlot)
    oldDump <- getDump
    result <- rollbackTip slotToRollbackTo
    assert
        "Rollback at or after tip should succeed"
        result
    newDump <- getDump
    assert
        "Rollback at or after tip should be no-op"
        $ newDump == oldDump

-- | Property: rolling back to a slot before tip undoes the changes made after it
-- given we know some snapshots of the database at various slots,
-- rolling back after or at the first one will restore the database to that state
propertyRollbackAfterBeforeTipUndoesChanges
    :: PropertyConstraints m slot key value
    => NonEmpty (WithOrigin slot, Dump slot key value)
    -> PropertyWithExpected m slot key value ()
propertyRollbackAfterBeforeTipUndoesChanges history@((pastSlot, _) :| _) = do
    Generator{genSlot} <- asksGenerator
    current <- getDump
    tip <- getTip
    slot <-
        pick
            $ genWithOrigin genSlot
                `suchThat` (<= tip)
                `suchThat` (>= pastSlot)
    let dump = findValue slot Nothing $ toList history <> [(tip, current)]
    result <- rollbackTip slot
    assert
        "Rollback to a slot before tip should succeed"
        result
    finalDump <- getDump
    assert
        "Rollback moves the database to a previous state"
        $ dump == Just finalDump

-- | Property: forwarding finality before finality is a no-op
-- forwarding finality should work only if the new slot is after the current finality
propertyForwardFinalityBeforeFinalityIsNoOp
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyForwardFinalityBeforeFinalityIsNoOp = do
    Generator{genSlot} <- asksGenerator
    finalityBefore <- getFinality
    case finalityBefore of
        Origin -> pure ()
        At finalitySlot -> do
            slot <- pick $ genSlot `suchThat` (<= finalitySlot)
            oldDump <- getDump
            forwardFinality slot
            newDump <- getDump
            assert
                "Forwarding finality slot before or at finality should be no-op"
                $ newDump == oldDump

-- | Property: forwarding finality after finality reduces the rollback window
-- given we know some snapshots of the database at various slots,
-- forwarding finality to a slot after the current finality
-- rolling back to the old finality should fail
-- rolling back to the new finality should restore the database to that state
propertyForwardFinalityAfterFinalityReduceTheRollbackWindow
    :: PropertyConstraints m slot key value
    => NonEmpty (WithOrigin slot, Dump slot key value)
    -> PropertyWithExpected m slot key value ()
propertyForwardFinalityAfterFinalityReduceTheRollbackWindow history@((pastSlot, _) :| _) = do
    Generator{genSlot} <- asksGenerator
    finalityBefore <- getFinality
    tip <- getTip
    current <- getDump
    slot <-
        pick
            $ genSlot
                `suchThat` (\s -> At s <= tip)
                `suchThat` (\s -> At s >= pastSlot)
                `suchThat` (\s -> At s > finalityBefore)
    let dump = findValue (At slot) Nothing $ toList history <> [(tip, current)]
    forwardFinality slot
    failure <- rollbackTip finalityBefore
    assert
        "Rollback at old finality should fail"
        $ not failure
    success <- rollbackTip (At slot)
    assert
        "Rollback to forwarded finality should succeed"
        success
    assertingJust "Should have a dump at the new finality slot" dump $ \oldDump -> do
        finalDump <- getDump
        assert
            "Rollback to forwarded finality moves the database to a previous state"
            $ finalDump == oldDump{dumpFinality = At slot}

-- | Property: forwarding finality beyond tip sets finality as tip
-- finality is set to tip
-- opposites are cleared
-- associations remain as is
propertyForwardFinalityBeyondTipSetsFinalityAsTip
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyForwardFinalityBeyondTipSetsFinalityAsTip = do
    Generator{genSlot} <- asksGenerator
    tip <- getTip
    slot <- case tip of
        Origin -> pick genSlot
        At tipSlot -> pick $ genSlot `suchThat` (> tipSlot)
    current <- getDump
    forwardFinality slot
    newFinality <- getFinality
    assert
        "Forwarding finality slot beyond tip should set it to tip"
        $ newFinality == tip
    updated <- getDump
    assert
        "New database just has finality updated, other contents remain the same"
        $ updated == current{dumpFinality = tip}

-- | Property: rolling back finality truncates the database
-- all contents are removed
-- tip and finality are set to Origin
-- opposites are cleared
propertyRollbackFinalityTruncatesTheDatabase
    :: PropertyConstraints m slot key value
    => PropertyWithExpected m slot key value ()
propertyRollbackFinalityTruncatesTheDatabase = do
    rollbackFinality
    dump <- getDump
    assert
        "After rolling back finality, the database should be empty"
        $ dump
            == Dump
                { dumpAssocs = []
                , dumpFinality = Origin
                , dumpTip = Origin
                }
