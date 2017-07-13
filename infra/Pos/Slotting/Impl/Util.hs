-- | Utilities used by slotting implementations.

module Pos.Slotting.Impl.Util
       ( slotFromTimestamp
       ) where

import           Universum

import           Data.Time.Units             (convertUnit)
import           NTP.Example                 ()

import           Pos.Core.Types              (SlotId (..), Timestamp (..),
                                              mkLocalSlotIndex)

import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (EpochSlottingData (..), findMatchingEpoch)

-- | Compute current slot from current timestamp based on data
-- provided by 'MonadSlotsData'.
slotFromTimestamp
    :: MonadSlotsData m
    => Timestamp -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do
  li <- getEpochLastIndex
  me <- findMatchingEpoch approxCurTime li getEpochSlottingData
  return $ do
    (ei, EpochSlottingData{..}) <- me
    slot <- either (const Nothing) Just $ mkLocalSlotIndex $ fromIntegral $ (getTimestamp approxCurTime - getTimestamp esdStart) `div` convertUnit esdSlotDuration
    return $ SlotId ei slot
