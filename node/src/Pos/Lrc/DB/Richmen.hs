{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Richmen part of LRC DB.

module Pos.Lrc.DB.Richmen
       (
       -- * Initialization
         prepareLrcRichmen

       -- * Concrete instances
       -- ** Ssc
       , RCSsc
       , getRichmenSsc

       -- ** US
       , RCUs
       , getRichmenUS

       -- ** Delegation
       , RCDlg
       , getRichmenDlg

       -- * Exported for tests
       , richmenComponents
       ) where

import           Universum

import qualified Data.HashMap.Strict         as HM

import           Pos.Binary.Core             ()
import           Pos.Constants               (genesisHeavyDelThd)
import           Pos.Context                 (GenesisUtxo, genesisStakesM)
import           Pos.Core                    (Coin, EpochIndex, GenesisWStakeholders,
                                              StakeholderId)
import           Pos.DB.Class                (MonadDB, MonadDBRead)
import           Pos.Lrc.Class               (RichmenComponent (..),
                                              SomeRichmenComponent (..),
                                              someRichmenComponent)
import           Pos.Lrc.DB.RichmenBase      (getRichmen, getRichmenP, putRichmenP)
import           Pos.Lrc.Logic               (RichmenType (..), findRichmenPure)
import           Pos.Lrc.Types               (FullRichmenData, RichmenSet)
import           Pos.Ssc.RichmenComponent    (RCSsc, getRichmenSsc)
import           Pos.Update.RichmenComponent (RCUs, getRichmenUS)
import           Pos.Util.Util               (HasLens', getKeys)

----------------------------------------------------------------------------
-- Initialization
----------------------------------------------------------------------------

prepareLrcRichmen ::
       ( MonadReader ctx m
       , HasLens' ctx GenesisUtxo
       , HasLens' ctx GenesisWStakeholders
       , MonadDB m
       )
    => m ()
prepareLrcRichmen = do
    genesisDistribution <- HM.toList <$> genesisStakesM
    mapM_ (prepareLrcRichmenDo genesisDistribution) richmenComponents
  where
    prepareLrcRichmenDo distr (SomeRichmenComponent proxy) =
        whenNothingM_ (getRichmenP proxy 0) $
            putRichmenP proxy 0 (computeInitial distr proxy)

computeInitial
    :: RichmenComponent c
    => [(StakeholderId, Coin)] -> Proxy c -> FullRichmenData
computeInitial initialDistr proxy =
    findRichmenPure
        initialDistr
        (rcInitialThreshold proxy)
        richmenType
  where
    richmenType
        | rcConsiderDelegated proxy = RTDelegation mempty
        | otherwise = RTUsual

----------------------------------------------------------------------------
-- Instances. They are here, because we want to have a DB schema in Pos.DB
----------------------------------------------------------------------------

richmenComponents :: [SomeRichmenComponent]
richmenComponents =
    [ someRichmenComponent @RCSsc
    , someRichmenComponent @RCUs
    , someRichmenComponent @RCDlg
    ]

----------------------------------------------------------------------------
-- Delegation instance
----------------------------------------------------------------------------

data RCDlg

instance RichmenComponent RCDlg where
    type RichmenData RCDlg = RichmenSet
    rcToData = getKeys . snd
    rcTag Proxy = "dlg"
    rcInitialThreshold Proxy = genesisHeavyDelThd
    rcConsiderDelegated Proxy = False

getRichmenDlg :: MonadDBRead m => EpochIndex -> m (Maybe RichmenSet)
getRichmenDlg epoch = getRichmen @RCDlg epoch
