{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( HasSlottingVar(..)
       , getSystemStartDefault
       , getSlottingDataDefault
       , waitPenultEpochEqualsDefault
       , getEpochSlottingDataDefault
       , getEpochLastIndexDefault
       , putEpochSlottingDataDefault
       ) where

import           Universum

import           Control.Monad.STM  (retry)

import           Pos.Core.Types     (EpochIndex, Timestamp)
import           Pos.Slotting.Types (EpochSlottingData, SlottingData,
                                     addEpochSlottingData, getLastEpochIndex,
                                     getPenultEpochIndex, lookupEpochSlottingData)

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

-- | System start and slotting data
class HasSlottingVar ctx where
    slottingTimestamp :: Lens' ctx Timestamp
    slottingVar :: Lens' ctx (TVar SlottingData)

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

type SlotsDefaultEnv ctx m =
    (MonadReader ctx m, HasSlottingVar ctx, MonadIO m)

getSystemStartDefault :: SlotsDefaultEnv ctx m => m Timestamp
getSystemStartDefault = view slottingTimestamp

getSlottingDataDefault :: SlotsDefaultEnv ctx m => m SlottingData
getSlottingDataDefault = atomically . readTVar =<< view slottingVar

waitPenultEpochEqualsDefault :: SlotsDefaultEnv ctx m => EpochIndex -> m ()
waitPenultEpochEqualsDefault target = do
    var <- view slottingVar
    atomically $ do
        penultEpoch <- getPenultEpochIndex <$> readTVar var
        when (penultEpoch /= target) retry

putEpochSlottingDataDefault :: SlotsDefaultEnv ctx m => EpochIndex -> EpochSlottingData -> m ()
putEpochSlottingDataDefault ei esd = do
    var <- view slottingVar
    atomically $ do
        sd <- readTVar var
        writeTVar var (addEpochSlottingData ei esd sd)

getEpochSlottingDataDefault :: SlotsDefaultEnv ctx m => EpochIndex -> m (Maybe EpochSlottingData)
getEpochSlottingDataDefault ei = do
    var <- view slottingVar
    atomically $ lookupEpochSlottingData ei <$> readTVar var

getEpochLastIndexDefault :: SlotsDefaultEnv ctx m => m EpochIndex
getEpochLastIndexDefault = do
    var <- view slottingVar
    atomically $ (fromMaybe 0 . getLastEpochIndex) <$> readTVar var


-- AJ: MERGE: ALL THESE NEW DEFAULT FUNCTIONS NEED TO BE USED SOMEWHERE

-- getEpochSlottingDataDefault :: SlotsDefaultEnv ctx m => EpochIndex -> m ()
-- getEpochSlottingDataDefault ei = do
--     var <- view slottingVar
--     atomically $ lookupEpochSlottingData ei <$> readTVar var
--
-- putEpochSlottingDataDefault :: SlotsDefaultEnv ctx m => EpochIndex -> EpochSlottingData -> m ()
-- putEpochSlottingDataDefault ei esd = do
--      var <- view slottingVar
--      atomically $ do
--          sd <- readTVar var
--          writeTVar var (addEpochSlottingData ei esd sd)
