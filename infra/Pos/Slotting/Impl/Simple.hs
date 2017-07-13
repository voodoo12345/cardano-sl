{-# LANGUAGE TypeFamilies #-}

-- | Simple implementation of slotting.

module Pos.Slotting.Impl.Simple
       ( SimpleSlottingMode
       , getCurrentSlotSimple
       , getCurrentSlotBlockingSimple
       , currentTimeSlottingSimple
       ) where

import           Universum

import           Mockable                    (CurrentTime, Mockable, currentTime)
import           NTP.Example                 ()

import           Pos.Core.Types              (SlotId (..), Timestamp (..))
import           Pos.Slotting.Impl.Util      (slotFromTimestamp)
import           Pos.Slotting.MemState.Class (MonadSlotsData (..))

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type SimpleSlottingMode m = (Mockable CurrentTime m, MonadSlotsData m)

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

getCurrentSlotSimple :: SimpleSlottingMode m => m (Maybe SlotId)
getCurrentSlotSimple = currentTimeSlottingSimple >>= slotFromTimestamp

getCurrentSlotBlockingSimple :: SimpleSlottingMode m => m SlotId
getCurrentSlotBlockingSimple = do
    lastIndex <- getEpochLastIndex
    getCurrentSlotSimple >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitPenultEpochEquals lastIndex
            getCurrentSlotBlockingSimple

currentTimeSlottingSimple :: SimpleSlottingMode m => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime
