-- | Specification for Pos.Ssc.GodTossing.Toss.Pure

module Test.Pos.Ssc.GodTossing.Toss.PureSpec
       ( spec
       ) where

import           Universum

import qualified Crypto.Random                     as Rand
import           Data.Default                      (def)
import           Test.Hspec                        (Spec, describe)
import           Test.Hspec.QuickCheck             (modifyMaxSuccess, prop)
import           Test.QuickCheck                   (Arbitrary (..), Gen, Property, forAll,
                                                    (===))
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import           Pos.Core                          (EpochOrSlot, HasCoreConstants,
                                                    StakeholderId, addressHash,
                                                    giveStaticConsts)
import qualified Pos.Ssc.GodTossing                as Toss

spec :: Spec
spec = giveStaticConsts $ describe "Toss" $ do
    let smaller n = modifyMaxSuccess (const n)
    describe "PureToss" $ smaller 30 $ do
        prop "Adding and deleting a signed commitment in the 'PureToss' monad is the\
             \ same as doing nothing"
             putDelCommitment
        prop "Adding and deleting an opening in the 'PureToss' monad is the same as doing\
             \ nothing"
             putDelOpening
        prop "Adding and deleting a share in the 'PureToss' monad is the same as doing\
             \ nothing"
             putDelShare

data TossAction
    = PutCommitment Toss.SignedCommitment
    | PutOpening StakeholderId Toss.Opening
    | PutShares StakeholderId Toss.InnerSharesMap
    | PutCertificate Toss.VssCertificate
    | ResetCO
    | ResetShares
    | DelCommitment StakeholderId
    | DelOpening StakeholderId
    | DelShares StakeholderId
    | SetEpochOrSlot EpochOrSlot
    deriving (Show, Eq, Generic)

instance HasCoreConstants => Arbitrary TossAction where
    arbitrary = genericArbitrary
    shrink = genericShrink

actionToMonad :: Toss.MonadToss m => TossAction -> m ()
actionToMonad (PutCommitment sc)   = Toss.putCommitment sc
actionToMonad (PutOpening sid o)   = Toss.putOpening sid o
actionToMonad (PutShares sid ism)  = Toss.putShares sid ism
actionToMonad (PutCertificate v)   = Toss.putCertificate v
actionToMonad ResetCO              = Toss.resetCO
actionToMonad ResetShares          = Toss.resetShares
actionToMonad (DelCommitment sid)  = Toss.delCommitment sid
actionToMonad (DelOpening sid)     = Toss.delOpening sid
actionToMonad (DelShares sid)      = Toss.delShares sid
actionToMonad (SetEpochOrSlot eos) = Toss.setEpochOrSlot eos

emptyTossSt :: Toss.GtGlobalState
emptyTossSt = def

perform :: HasCoreConstants => [TossAction] -> Toss.PureToss ()
perform = mapM actionToMonad

-- | Type synonym used for convenience. This quintuple is used to pass the randomness
-- needed to run 'PureToss' actions to the testing property.
type TossTestInfo = (Word64, Word64, Word64, Word64, Word64)

-- | Operational equivalence operator in the 'PureToss' monad. To be used when
-- equivalence between two sequences of actions in 'PureToss' is to be tested/proved.
(==^) :: HasCoreConstants => [TossAction] -> [TossAction] -> TossTestInfo -> Property
t1 ==^ t2 = \ttInfo ->
    forAll (arbitrary :: Gen [TossAction]) $ \prefix ->
    forAll (arbitrary :: Gen [TossAction]) $ \suffix ->
        let applyAction x =
                view _2 .
                fst . Rand.withDRG (Rand.drgNewTest ttInfo) .
                Toss.runPureToss emptyTossSt $ (perform $ prefix ++ x ++ suffix)
        in applyAction t1 === applyAction t2

putDelCommitment :: HasCoreConstants => Toss.SignedCommitment -> TossTestInfo -> Property
putDelCommitment sc = [PutCommitment sc, DelCommitment $ addressHash $ sc ^. _1] ==^ []

putDelOpening
    :: HasCoreConstants
    => StakeholderId
    -> Toss.Opening
    -> TossTestInfo
    -> Property
putDelOpening sid o = [PutOpening sid o, DelOpening sid] ==^ []

putDelShare
    :: HasCoreConstants
    => StakeholderId
    -> Toss.InnerSharesMap
    -> TossTestInfo
    -> Property
putDelShare sid ism = [PutShares sid ism, DelShares sid] ==^ []
