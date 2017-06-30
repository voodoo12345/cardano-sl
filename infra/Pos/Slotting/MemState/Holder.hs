{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( HasSlottingVar(..)
       , getSystemStartDefault
       , getSlottingDataDefault
       , waitPenultEpochEqualsDefault
       , putSlottingDataDefault
       ) where

import           Universum

import           Control.Monad.STM  (retry)

import           Pos.Core.Types     (EpochIndex, Timestamp)
import           Pos.Slotting.Types (SlottingData, addEpochSlottingData,
                                     getPenultEpochIndex, lookupEpochSlottingData, getLastEpochIndex)

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
        penultEpoch <- sdPenultEpochIndex <$> readTVar var
        when (penultEpoch /= target) retry

-- AJ: MERGE: ALL THESE NEW DEFAULT FUNCTIONS NEED TO BE USED SOMEWHERE

-- AJ: MERGE: THIS PROBABLY NEEDS TO GO
putSlottingDataDefault :: SlotsDefaultEnv ctx m => SlottingData -> m ()
putSlottingDataDefault sd = do
    var <- view slottingVar
    atomically $ do
        penultEpoch <- sdPenultEpoch <$> readTVar var
        when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd

getEpochLastIndexDefault :: SlotsDefaultEnv ctx m => m ()
getEpochLastIndexDefault = do
    var <- view slottingVar
    atomically $ (fromMaybe 0 . getLastEpochIndex) <$> readTVar var

getEpochSlottingDataDefault :: SlotsDefaultEnv ctx m => EpochIndex -> m ()
getEpochSlottingDataDefault ei = do
    var <- view slottingVar
    atomically $ lookupEpochSlottingData ei <$> readTVar var

putEpochSlottingDataDefault :: SlotsDefaultEnv ctx m => EpochIndex -> EpochSlottingData -> m ()
putEpochSlottingDataDefault ei esd = do
     var <- view slottingVar
     atomically $ do
         sd <- readTVar var
         writeTVar var (addEpochSlottingData ei esd sd)
