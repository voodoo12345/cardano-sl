{-# LANGUAGE TypeFamilies #-}

module Pos.Slotting.MemState.Class
       ( MonadSlotsData (..)
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Universum

import           Pos.Core.Types      (EpochIndex, Timestamp)
import           Pos.Slotting.Types  (EpochSlottingData)

-- | 'MonadSlotsData' provides access to data necessary for slotting to work.
class Monad m => MonadSlotsData m where

    getSystemStart :: m Timestamp

    waitPenultEpochEquals :: EpochIndex -> m ()

    getEpochLastIndex :: m EpochIndex

    getEpochSlottingData :: EpochIndex -> m (Maybe EpochSlottingData)

    putEpochSlottingData :: EpochIndex -> EpochSlottingData -> m ()

instance {-# OVERLAPPABLE #-}
    (MonadSlotsData m, MonadTrans t, Monad (t m)) =>
        MonadSlotsData (t m) where
    getSystemStart = lift getSystemStart
    waitPenultEpochEquals = lift . waitPenultEpochEquals
    getEpochLastIndex = lift getEpochLastIndex
    getEpochSlottingData = lift . getEpochSlottingData
    putEpochSlottingData = (lift .) . putEpochSlottingData
