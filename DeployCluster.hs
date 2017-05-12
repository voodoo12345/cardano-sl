#!/usr/bin/env stack
-- stack runghc --package optparse-simple --package shell-conduit --package transformers

{-# LANGUAGE RecordWildCards #-}

import           Options.Applicative.Simple  (Parser,
                                              simpleOptions, empty,
                                              flag', strOption, option,
                                              value, showDefault, auto, long, metavar, help,
                                              (<|>))
import           Data.Conduit.Shell
import           Data.Conduit.Shell.Segments (strings)
import           Data.List                   (intersperse)
import           Data.Monoid                 ((<>))
import           Data.Char                   (toLower)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad               (unless, when)
import           System.FilePath.Posix       ((</>))
import           System.Exit                 (die)

main :: IO ()
main = do
    (options, ()) <- simpleOptions "0.1"
                                   "Deploy new Cardano SL cluster from scratch."
                                   ""
                                   optionsParser
                                   empty
    run $ deploymentScript options

deploymentScript :: Options -> Segment ()
deploymentScript Options{..} = do
    echo "Assumed that:"
    echo " 1. you have an SSH-access to 'cardano-deployer' server,"
    echo " 2. current durectory is a root of 'cardano-sl' repository,"
    echo " 3. you're using branch corresponding to an issue the new cluster is for,"
    echo " 4. you already changed fundamental constants for a new cluster in 'cardano-sl/core/constants-*.yaml', if required."
    echo "yes/no?"
    continueIfYes
    showInitialInfoAboutCluster
    when itIsProductionCluster buildCardanoSLInProdMode
    makeSureClusterNameIsUnique
    cloneBaseForNewCluster
    createMainConfigYAML
    if itIsProductionCluster
        then do
            generateNewKeys
            commitAndPushNewGenesisFiles
            uploadGeneratedKeysToCluster
            preparingGenerateScript
        else do
            echo ""
            echo "It's dev-cluster, we shouldn't generate new keys and copy them to nodes."
            preparingGenerateScript
    {-
    generateDeployment
    cluster Create
    cluster Build
    prepareNodesForDeployment
    cluster Deploy
    when itIsProductionCluster copyGeneratedKeysToNodes
    removeNodesDatabases
    setSystemStartTime
    cluster Stop
    cluster Deploy
    cluster Start
    -}
  where
    -- Basic settings --------------------------------------------------------------------
    nixConfig            = "config.nix"
    prodConfigName       = "production.yaml"
    devConfigName        = "config.yaml"
    generateScript       = "generate.sh"
    cardanoNix           = "deployments/cardano.nix"
    genesisKeysDirPrefix = "genesis-qanet-"
    nodeFilesRoot        = "/var/lib/cardano-node/"
    nodeUser             = "cardano-node"
    clusterName          = map toLower issueId
    clusterRoot          = deployerUserHome </> clusterName
    newConfigYAML        = if itIsProductionCluster then prodConfigName else devConfigName
    pathToCardanoNix     = clusterRoot </> cardanoNix
    pathToNixConfig      = clusterRoot </> nixConfig
    pathToNewConfigYAML  = clusterRoot </> newConfigYAML
    pathToPkgs           = clusterRoot </> "pkgs"
    pathToGenerateScript = pathToPkgs </> generateScript
    deployerUserHome     = "/home" </> deployerUser
    deployerServer       = deployerUser <> "@35.156.156.28"
    --------------------------------------------------------------------------------------

    runCommandOnDeployer = ssh deployerServer
    
    getCurrentCommit = head <$> strings (git "rev-parse" "HEAD")
    getCurrentBranch = head <$> strings (git "branch" $| sed "-n" "-e" "s/^\\* \\(.*\\)/\\1/p")   

    continueIfYes =
        liftIO getLine >>= \maybeYes -> unless (maybeYes == "yes") . liftIO . die $ "Abort."

    showInitialInfoAboutCluster = do
        nameOfCurrentBranch <- getCurrentBranch
        echo ""
        echo "Deploying a new cluster" clusterName
             "with" (show numberOfNodes) "nodes,"
             mode
             "using branch is" nameOfCurrentBranch "..."
      where
        mode = if itIsProductionCluster then "prod-mode," else "dev-mode,"

    buildCardanoSLInProdMode = do
        echo ""
        echo "We have to build 'cardano-sl' in prod-mode, for generating new keys correctly..."
        shell "CSL_SYSTEM_TAG=linux stack build --flag cardano-sl-core:-dev-mode"

    makeSureClusterNameIsUnique = do
        echo ""
        echo "Make sure that cluster with such name doesn't exist..."
        maybeExists <- strings (runCommandOnDeployer $ "if [ -d " <> clusterName <> " ]; then echo '" <> yes <> "'; else echo '" <> no <> "'; fi")
        unless (head maybeExists == no) . liftIO . die $ "Sorry, cluster " <> clusterName <> " already exists, please use unique name."
      where
        yes = "yes"
        no  = "no"

    cloneBaseForNewCluster = do
        echo ""
        echo "Clone base for a new cluster" clusterName "..."
        runCommandOnDeployer $ "git clone -q https://github.com/input-output-hk/iohk-nixops.git " <> clusterName

    createMainConfigYAML = do
        echo ""
        echo "Create a new config file," newConfigYAML "..."
        runCommandOnDeployer $ "printf \"" <> newConfigTemplate <> "\" > " <> pathToNewConfigYAML
      where
        newConfigTemplate = concat $
            intersperse "\n" [ "deploymentName: " <> clusterName
                             -- TODO: Probably 'nixPath' should be defined in CLI-option too.
                             , "nixPath: nixpkgs=https://github.com/NixOS/nixpkgs/archive/8c6e9177a468618a63836f1e28576e682f93f216.tar.gz"
                             , "deploymentFiles:"
                             , "  - " <> cardanoNix
                             , "nixopsExecutable: nixops"
                             ]

    generateNewKeys = do
        echo ""
        echo "Generate new keys for a cluster's nodes..."
        -- TODO: Probably N-value should be defined in CLI-option too.
        shell $ "M=" <> show numberOfNodes <> " N=120 ./util-scripts/generate-genesis.sh"
    
    commitAndPushNewGenesisFiles = do
        echo ""
        echo "Commit and push updated genesis.* files..."
        git "commit" "-a" "-m" ("[" <> issueId <> "] Update genesis.* files for new keys.")
        getCurrentBranch >>= git "push" "origin"

    uploadGeneratedKeysToCluster = do
        echo ""
        echo "Upload generated keys to new cluster (after deployment these keys will be copied to nodes)..."
        runCommandOnDeployer $ "cd " <> clusterRoot <> " && CUR_DATE=$(date +%F) && mkdir " <> genesisKeysDirPrefix <> "$CUR_DATE"
        scp "-r" (genesisKeysDirPrefix <> "*/nodes") (deployerServer <> ":" <> clusterRoot)

    copyGeneratedKeysToNodes = do
        echo ""
        echo "Copy generated keys from a cluster to all nodes..."
        runCommandOnDeployer $ concat $
            intersperse "&&" [ "cd " <> genesisKeysDirPrefix <> "`date +%F`"
                             ,    "for i in {0.." <> show numberOfNodes <> "};"
                               <> "do nixops scp -d " <> clusterName <> " --to node$i key$((i+1)) " <> nodeFilesRoot <> "key$((i+1)).sk;"
                               <> "done"
                             , "nixops ssh-for-each -d " <> clusterName
                               <> " 'chown " <> nodeUser <> ":" <> nodeUser <> " " <> nodeFilesRoot <> "key*.sk'"
                             ]

    preparingGenerateScript = do
        echo ""
        echo "Update 'cardano-sl' commit in" pathToGenerateScript "..."
        currentCommit <- getCurrentCommit
        echo " Current commit is" currentCommit
        runCommandOnDeployer $ replaceCardanoSLCommit currentCommit
        echo ""
        echo "If you need to update commits for other packages, please do it now."
        echo "After you finished (or if you don't need to change commits), type 'yes'. Type 'no' to abort script."
        echo "yes/no?"
        continueIfYes
      where
        replaceCardanoSLCommit commit =
            "sed -i -e 's/cardano-sl\\.git\\([ ]*\\)\\([a-f0-9][a-f0-9]*\\)/cardano-sl\\.git " <> commit <> "/g' " <> pathToGenerateScript

    generateDeployment = do
        echo ""
        echo "Run generate.sh..."
        runCommandOnDeployer $ "cd " <> pathToPkgs <> " && ./" <> generateScript

    prepareNodesForDeployment = do
        echo ""
        echo "Now you have to prepare cluster's nodes for deployment. This step cannot be automated because of"
        echo "specific settings for AWS regions for nodes, elastic IPs, etc. So please go to cluster, open" pathToCardanoNix
        echo "file and (un)comment corresponding 'genAttrs'-sections. After you finished, type 'yes'. Type 'no' to abort script."
        echo "yes/no?"
        continueIfYes
    
    removeNodesDatabases = do
        echo ""
        echo "Remove databases on all nodes..."
        runCommandOnDeployer $ "nixops ssh-for-each -d " <> clusterName <> " 'rm -R " <> nodeFilesRoot <> "node-db'"

    setSystemStartTime = do
        echo ""
        echo "Set system start time..."
        runCommandOnDeployer $ "START=$(( $(date +%s)+120 )) && sed -i \"s/systemStart[ ]*=[ ]*[0-9]*;/systemStart = $START;/g\" " <> pathToNixConfig

    cluster action = do
        echo ""
        echo (show action) "cluster..."
        runCommandOnDeployer $ "cd " <> clusterRoot <> " && ./CardanoCSL.hs -c " <> newConfigYAML <> " " <> show action

-- | What can we do with cluster?
data ClusterAction 
    = Create
    | Build
    | Deploy
    | Start
    | Stop

instance Show ClusterAction where
    show Create  = "create"
    show Build   = "build"
    show Deploy  = "deploy"
    show Start   = "start"
    show Stop    = "stop"

-- | CLI-options for deployer.
data Options = Options
    { itIsProductionCluster :: Bool
    , deployerUser          :: String
    , issueId               :: String
    , numberOfNodes         :: Int
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> ( flag' True  (long "prod") 
      <|> flag' False (long "dev") )
    <*> strOption (
         long       "user"
      <> metavar    "USER-NAME"
      <> value      "staging"
      <> showDefault
      <> help       "User name on 'cardano-deployer' server." )
    <*> strOption (
         long       "issue"
      <> metavar    "ISSUE-ID"
      <> help       "ID of the issue the new cluster is for, for example CSL-1093." )
    <*> option auto (
         long       "nodes"
      <> metavar    "NUMBER"
      <> help       "Number of nodes in a cluster, from 1 to 100." )
