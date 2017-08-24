{-# LANGUAGE TypeFamilies #-}

-- | Type class necessary for Transaction processing (Txp)
-- and some useful getters and setters.

module Pos.Txp.MemState.Class
       ( MonadTxpMem
       , askTxpMem
       , askTxpMemAndMetrics
       , TxpHolderTag
       , getUtxoModifier
       , getLocalTxsNUndo
       , getMemPool
       , getLocalTxs
       , getLocalTxsMap
       , getTxpExtra
       , modifyTxpLocalData
       , setTxpLocalData
       , clearTxpMemPool
       , withMemPoolLock
       ) where

import           Universum

import qualified Control.Concurrent.STM      as STM
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.Default                (Default (def))
import qualified Data.HashMap.Strict         as HM
import           Ether.Internal              (HasLens (..))
import           Mockable                    (CurrentTime, Mockable, currentTime)
import           System.Wlog                 (WithLogger, getLoggerName, logDebug, usingLoggerName)

import           Pos.Txp.Core.Types          (TxAux, TxId, TxUndo)
import           Pos.Txp.MemState.Types      (GenericTxpLocalData (..),
                                              GenericTxpLocalDataPure, TxpMetrics (..))
import           Pos.Txp.Toil.Types          (MemPool (..), UtxoModifier)
import qualified Pos.Util.PrioLock           as PL

data TxpHolderTag

-- | More general version of @MonadReader (GenericTxpLocalData mw, TxpMetrics) m@.
type MonadTxpMem ext ctx m
     = ( MonadReader ctx m
       , HasLens TxpHolderTag ctx (GenericTxpLocalData ext, TxpMetrics)
       , Mockable CurrentTime m
       )

askTxpMem :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext)
askTxpMem = fst <$> view (lensOf @TxpHolderTag)

askTxpMemAndMetrics :: MonadTxpMem ext ctx m => m (GenericTxpLocalData ext, TxpMetrics)
askTxpMemAndMetrics = view (lensOf @TxpHolderTag)

getTxpLocalData
    :: (MonadIO m, MonadTxpMem e ctx m)
    => (GenericTxpLocalData e -> STM.STM a) -> m a
getTxpLocalData getter = askTxpMem >>= \ld -> atomically (getter ld)

getUtxoModifier
    :: (MonadTxpMem e ctx m, MonadIO m)
    => m UtxoModifier
getUtxoModifier = getTxpLocalData (STM.readTVar . txpUtxoModifier)

getLocalTxsMap
    :: (MonadIO m, MonadTxpMem e ctx m)
    => m (HashMap TxId TxAux)
getLocalTxsMap = _mpLocalTxs <$> getMemPool

getLocalTxs
    :: (MonadIO m, MonadTxpMem e ctx m)
    => m [(TxId, TxAux)]
getLocalTxs = HM.toList <$> getLocalTxsMap

getLocalTxsNUndo
    :: (MonadIO m, MonadTxpMem e ctx m)
    => m ([(TxId, TxAux)], HashMap TxId TxUndo)
getLocalTxsNUndo =
    getTxpLocalData $ \TxpLocalData {..} ->
        (,) <$>
        (HM.toList . _mpLocalTxs <$> STM.readTVar txpMemPool) <*>
        STM.readTVar txpUndos

getMemPool :: (MonadIO m, MonadTxpMem e ctx m) => m MemPool
getMemPool = getTxpLocalData (STM.readTVar . txpMemPool)

getTxpExtra :: (MonadIO m, MonadTxpMem e ctx m) => m e
getTxpExtra = getTxpLocalData (STM.readTVar . txpExtra)

-- | Perform an action while taking a lock on the mempool.
--
-- Will also add some timing information (wait time, time to complete
-- the action, size of the mempool after completing the action) to EKG
-- and the JSON logs.
withMemPoolLock :: (WithLogger m, MonadIO m, MonadBaseControl IO m, MonadTxpMem ext ctx m)
    => String
    -> PL.Priority
    -> m a
    -> m a
withMemPoolLock reason prio cont = askTxpMemAndMetrics >>= \(TxpLocalData{..}, TxpMetrics{..}) -> do
    lname <- getLoggerName
    let doLog = liftIO . usingLoggerName lname
    doLog $ txpMetricsWait reason
    timeBeginWait <- currentTime
    PL.withPrioLock txpMemPoolLock prio $ do
        logDebug . toText $ "Taking MemPoolLock for " ++ reason
        timeEndWait <- currentTime
        doLog $ txpMetricsAcquire (timeEndWait - timeBeginWait)
        timeBeginModify <- currentTime
        res <- cont
        timeEndModify <- currentTime
        newMemPoolSize <- _mpSize <$> (atomically $ STM.readTVar txpMemPool)
        doLog $ txpMetricsRelease (timeEndModify - timeBeginModify) newMemPoolSize
        logDebug . toText $ "Releasing MemPoolLock for " ++ reason
        pure res

-- | Modify the contents of the mempool.
--
-- While the modification is performed using STM, so that concurrent
-- updates are performed atomically, under contention, we have the
-- danger of live locking.  To prevent this, we use a lock via
-- 'withMemPoolLock'.
modifyTxpLocalData
    :: (WithLogger m, MonadIO m, MonadBaseControl IO m, MonadTxpMem ext ctx m)
    => String
    -> (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext))
    -> PL.Priority
    -> m a
modifyTxpLocalData reason f prio = withMemPoolLock reason prio $ modifyTxpLocalData_ f


-- | Version of 'modifyTxpLocalData' that does not take the lock.
modifyTxpLocalData_
    :: (WithLogger m, MonadIO m, MonadBaseControl IO m, MonadTxpMem ext ctx m)
    => (GenericTxpLocalDataPure ext -> (a, GenericTxpLocalDataPure ext))
    -> m a
modifyTxpLocalData_ f =
    askTxpMemAndMetrics >>= \(TxpLocalData{..}, TxpMetrics{..}) -> atomically $ do
    curUM  <- STM.readTVar txpUtxoModifier
    curMP  <- STM.readTVar txpMemPool
    curUndos <- STM.readTVar txpUndos
    curTip <- STM.readTVar txpTip
    curExtra <- STM.readTVar txpExtra
    let (res, (newUM, newMP, newUndos, newTip, newExtra))
            = f (curUM, curMP, curUndos, curTip, curExtra)
    STM.writeTVar txpUtxoModifier newUM
    STM.writeTVar txpMemPool newMP
    STM.writeTVar txpUndos newUndos
    STM.writeTVar txpTip newTip
    STM.writeTVar txpExtra newExtra
    pure res

-- | Set the mempool.
--
-- This function does not take the lock itself, and should be used
-- while the lock is still being held.
setTxpLocalData ::
       (WithLogger m, MonadIO m, MonadBaseControl IO m, MonadTxpMem ext ctx m)
    => GenericTxpLocalDataPure ext
    -> m ()
setTxpLocalData x = modifyTxpLocalData_ (const ((), x))

-- | Clear the mempool.
--
-- This function does not take the lock itself, and should be used
-- while the lock is still being held.
clearTxpMemPool ::
       ( WithLogger m
       , MonadIO m
       , MonadBaseControl IO m
       , MonadTxpMem ext ctx m
       , Default ext
       )
    => m ()
clearTxpMemPool = modifyTxpLocalData_ clearF
  where
    clearF (_, _, _, tip, _) = ((), (mempty, def, mempty, tip, def))
