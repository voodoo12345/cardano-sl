-- | Binary serialization of types from Slotting.

module Pos.Binary.Infra.Slotting
       (
       ) where

import           Data.Time.Units    (Millisecond)

import           Pos.Binary.Class   (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Binary.Core    ()
import           Pos.Core.Timestamp (Timestamp)
import           Pos.Slotting.Types (EpochSlottingData (..))

deriveSimpleBi ''EpochSlottingData [
    Cons 'EpochSlottingData [
        Field [| esdSlotDuration :: Millisecond |],
        Field [| esdStart        :: Timestamp   |]
    ]]
