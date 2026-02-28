{-# LANGUAGE NumericUnderscores #-}

{- |
Module      : Cardano.UTxOCSMT.E2E.Devnet
Description : Cardano-node subprocess for E2E tests
License     : Apache-2.0
-}
module Cardano.UTxOCSMT.E2E.Devnet
    ( withCardanoNode
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception
    ( bracket
    , onException
    )
import Control.Monad (unless, void)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Time.Clock
    ( NominalDiffTime
    , UTCTime
    , addUTCTime
    , getCurrentTime
    )
import Data.Time.Clock.POSIX
    ( utcTimeToPOSIXSeconds
    )
import Data.Time.Format
    ( defaultTimeLocale
    , formatTime
    )
import System.Directory
    ( copyFile
    , createDirectoryIfMissing
    , doesFileExist
    , getTemporaryDirectory
    , removePathForcibly
    )
import System.FilePath ((</>))
import System.IO
    ( BufferMode (..)
    , Handle
    , IOMode (..)
    , hClose
    , hSetBuffering
    , openFile
    )
import System.Posix.Files
    ( ownerReadMode
    , setFileMode
    )
import System.Process
    ( CreateProcess (..)
    , ProcessHandle
    , StdStream (..)
    , createProcess
    , proc
    , terminateProcess
    , waitForProcess
    )

{- | Run a @cardano-node@ subprocess using the
genesis files from @srcGenesis@. The callback
receives the node socket path.
The node and its temp directory are cleaned up
on exit.
-}
withCardanoNode
    :: FilePath
    -> (FilePath -> IO a)
    -> IO a
withCardanoNode srcGenesis action = do
    now <- getCurrentTime
    let startTime = addUTCTime startOffset now
    tmpDir <- prepareTmpDir srcGenesis startTime
    logH <- openFile (tmpDir </> "node.log") WriteMode
    hSetBuffering logH LineBuffering
    bracket
        (launchNode tmpDir logH)
        (cleanup tmpDir logH)
        $ \_ -> do
            let sock = tmpDir </> "node.sock"
            waitForSocket sock 300
            action sock
                `onException` dumpNodeLog
                    (tmpDir </> "node.log")

prepareTmpDir
    :: FilePath -> UTCTime -> IO FilePath
prepareTmpDir srcGenesis startTime = do
    sysTmp <- getTemporaryDirectory
    let tmpDir =
            sysTmp </> "cardano-utxo-csmt-e2e"
    removePathForcibly tmpDir
    createDirectoryIfMissing True tmpDir
    createDirectoryIfMissing
        True
        (tmpDir </> "db")
    createDirectoryIfMissing
        True
        (tmpDir </> "delegate-keys")
    let cp name =
            copyFile
                (srcGenesis </> name)
                (tmpDir </> name)
    cp "alonzo-genesis.json"
    cp "conway-genesis.json"
    cp "node-config.json"
    cp "topology.json"
    patchShelleyGenesis startTime srcGenesis tmpDir
    patchByronGenesis startTime srcGenesis tmpDir
    let srcKeys = srcGenesis </> "delegate-keys"
        dstKeys = tmpDir </> "delegate-keys"
        copyKey name = do
            copyFile
                (srcKeys </> name)
                (dstKeys </> name)
            setFileMode
                (dstKeys </> name)
                ownerReadMode
    copyKey "delegate1.kes.skey"
    copyKey "delegate1.vrf.skey"
    copyKey "delegate1.opcert"
    pure tmpDir

patchShelleyGenesis
    :: UTCTime -> FilePath -> FilePath -> IO ()
patchShelleyGenesis now srcDir dstDir = do
    let timeStr =
            BS8.pack
                $ formatTime
                    defaultTimeLocale
                    "%Y-%m-%dT%H:%M:%SZ"
                    now
    content <-
        BS.readFile
            (srcDir </> "shelley-genesis.json")
    let patched =
            replaceSubstring
                "PLACEHOLDER"
                timeStr
                content
    BS.writeFile
        (dstDir </> "shelley-genesis.json")
        patched

patchByronGenesis
    :: UTCTime -> FilePath -> FilePath -> IO ()
patchByronGenesis now srcDir dstDir = do
    let epoch =
            BS8.pack
                $ show
                    ( floor
                        (utcTimeToPOSIXSeconds now)
                        :: Int
                    )
    content <-
        BS.readFile
            (srcDir </> "byron-genesis.json")
    let patched =
            replaceSubstring
                "\"startTime\": 0"
                ("\"startTime\": " <> epoch)
                content
    BS.writeFile
        (dstDir </> "byron-genesis.json")
        patched

replaceSubstring
    :: BS.ByteString
    -> BS.ByteString
    -> BS.ByteString
    -> BS.ByteString
replaceSubstring needle replacement content =
    let (before, after) =
            BS.breakSubstring needle content
    in  if BS.null after
            then content
            else
                before
                    <> replacement
                    <> BS.drop
                        (BS.length needle)
                        after

launchNode
    :: FilePath -> Handle -> IO ProcessHandle
launchNode tmpDir logH = do
    let keysDir = tmpDir </> "delegate-keys"
        args =
            [ "run"
            , "--config"
            , tmpDir </> "node-config.json"
            , "--topology"
            , tmpDir </> "topology.json"
            , "--database-path"
            , tmpDir </> "db"
            , "--socket-path"
            , tmpDir </> "node.sock"
            , "--shelley-kes-key"
            , keysDir </> "delegate1.kes.skey"
            , "--shelley-vrf-key"
            , keysDir </> "delegate1.vrf.skey"
            , "--shelley-operational-certificate"
            , keysDir </> "delegate1.opcert"
            ]
        cp =
            (proc "cardano-node" args)
                { std_out = UseHandle logH
                , std_err = UseHandle logH
                }
    (_, _, _, ph) <- createProcess cp
    pure ph

cleanup
    :: FilePath
    -> Handle
    -> ProcessHandle
    -> IO ()
cleanup tmpDir logH ph = do
    terminateProcess ph
    void $ waitForProcess ph
    hClose logH
    removePathForcibly tmpDir
        `onException` pure ()

dumpNodeLog :: FilePath -> IO ()
dumpNodeLog logPath = do
    logContent <- BS.readFile logPath
    let allLines = BS8.lines logContent
        tailLines =
            drop
                (max 0 (length allLines - 50))
                allLines
    BS8.putStrLn
        ( BS8.unlines
            ( "=== Node log (last 50) ==="
                : tailLines
            )
        )

startOffset :: NominalDiffTime
startOffset = 5

waitForSocket :: FilePath -> Int -> IO ()
waitForSocket _ 0 =
    error
        "Timed out waiting for \
        \cardano-node socket"
waitForSocket path n = do
    exists <- doesFileExist path
    unless exists $ do
        threadDelay 100_000
        waitForSocket path (n - 1)
