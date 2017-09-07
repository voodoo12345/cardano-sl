-- | Types defining a command in Rubbish.

module Command.Types
       ( Command (..)
       , ProposeUpdateSystem (..)
       , SendMode (..)
       , SendToAllGenesisParams (..)
       , ProposeUpdateParams (..)
       , CmdCtx (..)
       ) where

import           Universum

import qualified Data.List.NonEmpty         as NE
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text
import           Prelude                    (read, show)
import           Serokell.Data.Memory.Units (Byte)
import           Serokell.Util.Parse        (parseIntegralSafe)
import           Text.Parsec                (many1, parse, parserFail, try, (<?>))
import           Text.Parsec.Char           (alphaNum, anyChar, digit, noneOf, oneOf,
                                             space, spaces, string)
import           Text.Parsec.Combinator     (eof, manyTill)
import           Text.Parsec.Text           (Parser)

import           Pos.Binary                 (decodeFull)
import           Pos.Client.CLI             (stakeholderIdParser)
import           Pos.Communication          (NodeId)
import           Pos.Core.Types             (ScriptVersion)
import           Pos.Core.Version           (parseBlockVersion, parseSoftwareVersion)
import           Pos.Crypto                 (AbstractHash, HashAlgorithm, PublicKey,
                                             SecretKey, decodeAbstractHash)
import           Pos.Genesis                (BalanceDistribution)
import           Pos.Txp                    (TxOut (..))
import           Pos.Types                  (AddrStakeDistribution (..), Address (..),
                                             BlockVersion, Coin, CoinPortion, EpochIndex,
                                             SoftwareVersion, decodeTextAddress, mkCoin,
                                             mkMultiKeyDistr, unsafeCoinPortionFromDouble)
import           Pos.Update                 (SystemTag, UpId, mkSystemTag)
import           Pos.Util.Util              (eitherToFail)

-- | Specify how transactions are sent to the network during
-- benchmarks using 'SendToAllGenesis'.
data SendMode =
      SendNeighbours -- ^ Send each transaction to every specified neighbour
    | SendRoundRobin -- ^ Send transactions to neighbours in a round-robin fashion
    | SendRandom     -- ^ Send each transaction to a randomly picked neighbour
    deriving Show

-- TODO: move somewhere!
data CmdCtx = CmdCtx
    { skeys               :: [SecretKey]
    , na                  :: [NodeId]
    , genesisBalanceDistr :: BalanceDistribution
    }

-- | Parameters for 'SendToAllGenesis' command.
data SendToAllGenesisParams = SendToAllGenesisParams
    { stagpDuration    :: !Int
    , stagpConc        :: !Int
    , stagpDelay       :: !Int
    , stagpMode        :: !SendMode
    , stagpTpsSentFile :: !FilePath
    } deriving (Show)

-- | Parameters for 'ProposeUpdate' command.
data ProposeUpdateParams = ProposeUpdateParams
    { puIdx             :: Int -- TODO: what is this? rename
    , puBlockVersion    :: BlockVersion
    , puScriptVersion   :: ScriptVersion
    , puSlotDurationSec :: Int
    , puMaxBlockSize    :: Byte
    , puSoftwareVersion :: SoftwareVersion
    , puUpdates         :: [ProposeUpdateSystem]
    } deriving (Show)

data Command
    = Balance Address
    | Send Int (NonEmpty TxOut)
    | SendToAllGenesis !SendToAllGenesisParams
    | Vote Int Bool UpId
    | ProposeUpdate !ProposeUpdateParams
    | Help
    | ListAddresses
    | DelegateLight !Int !PublicKey !EpochIndex !(Maybe EpochIndex) -- first and last epoch of psk ttl
    | DelegateHeavy !Int !PublicKey !EpochIndex -- last argument is current epoch
    | AddKeyFromPool !Int
    | AddKeyFromFile !FilePath
    | AddrDistr !PublicKey !AddrStakeDistribution
    | Quit
    deriving Show

data ProposeUpdateSystem = ProposeUpdateSystem
    { pusSystemTag     :: SystemTag
    , pusInstallerPath :: Maybe FilePath
    , pusBinDiffPath   :: Maybe FilePath
    } deriving Show
