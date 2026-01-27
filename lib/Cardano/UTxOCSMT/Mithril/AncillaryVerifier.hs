{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Cardano.UTxOCSMT.Mithril.AncillaryVerifier
Description : Ed25519 verification for Mithril ancillary files

This module implements Ed25519 signature verification for Mithril
ancillary files (ledger state snapshots). It verifies the
@ancillary_manifest.json@ file which contains:

* A mapping of file paths to their SHA256 hashes
* An Ed25519 signature over the manifest data

The verification process:
1. Parse the manifest JSON
2. Verify each file's SHA256 hash matches the manifest
3. Compute the manifest hash (SHA256 of sorted path||hash pairs)
4. Verify the Ed25519 signature over the manifest hash

Key format follows Mithril's JSON-hex encoding: the raw bytes are
serialized as a JSON array of integers, then hex-encoded.
-}
module Cardano.UTxOCSMT.Mithril.AncillaryVerifier
    ( -- * Verification
      verifyAncillaryManifest
    , AncillaryVerificationError (..)

      -- * Manifest types
    , AncillaryManifest (..)
    , ManifestSignature (..)

      -- * Key types
    , AncillaryVerificationKey (..)
    , parseVerificationKey

      -- * Testing
    , computeManifestHash
    , verifySignature
    , parseJsonHex
    )
where

import Control.Exception (Exception)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (SHA256 (..), hashFinalize, hashInit, hashUpdate)
import Crypto.PubKey.Ed25519 qualified as Ed25519
import Data.Aeson
    ( FromJSON (..)
    , eitherDecodeStrict
    , withObject
    , (.:)
    , (.:?)
    )
import Data.ByteArray (convert)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word8)
import System.Directory (doesFileExist)

-- | Ed25519 verification key for ancillary manifest verification
newtype AncillaryVerificationKey = AncillaryVerificationKey
    { unVerificationKey :: Ed25519.PublicKey
    }
    deriving stock (Show, Eq)

-- | Ed25519 signature from manifest
newtype ManifestSignature = ManifestSignature
    { unSignature :: Ed25519.Signature
    }
    deriving stock (Show, Eq)

-- | Ancillary manifest structure matching Mithril's format
data AncillaryManifest = AncillaryManifest
    { manifestData :: Map FilePath Text
    -- ^ File paths to SHA256 hashes (hex-encoded)
    , manifestSignature :: Maybe ManifestSignature
    -- ^ Optional Ed25519 signature
    }
    deriving stock (Show, Eq)

instance FromJSON AncillaryManifest where
    parseJSON = withObject "AncillaryManifest" $ \o -> do
        manifestData <- o .: "data"
        sigHex <- o .:? "signature"
        manifestSignature <- case sigHex of
            Nothing -> pure Nothing
            Just hex -> case parseHexSignature hex of
                Left err ->
                    fail $ "Invalid signature hex: " <> err
                Right sig ->
                    pure $ Just $ ManifestSignature sig
        pure AncillaryManifest{..}

-- | Parse a signature from plain hex format (used in manifest)
parseHexSignature :: Text -> Either String Ed25519.Signature
parseHexSignature hex =
    case Base16.decode (T.encodeUtf8 hex) of
        Left err -> Left $ "Hex decode failed: " <> err
        Right bs -> case Ed25519.signature bs of
            CryptoFailed e -> Left $ "Invalid Ed25519 signature: " <> show e
            CryptoPassed sig -> Right sig

-- | Errors during ancillary verification
data AncillaryVerificationError
    = -- | Manifest file not found
      ManifestNotFound FilePath
    | -- | Failed to parse manifest JSON
      ManifestParseError FilePath String
    | -- | A file listed in manifest was not found
      FileNotFound FilePath
    | -- | File hash does not match manifest
      HashMismatch
        FilePath
        -- ^ File path
        Text
        -- ^ Expected hash
        Text
        -- ^ Actual hash
    | -- | Manifest has no signature
      SignatureMissing
    | -- | Ed25519 signature verification failed
      SignatureInvalid
    deriving stock (Show, Eq)
    deriving anyclass (Exception)

{- | Parse a JSON-hex encoded bytestring

Mithril encodes keys/signatures as: hex(json([b0, b1, b2, ...]))
where each bi is a byte value 0-255.
-}
parseJsonHex :: Text -> Either String ByteString
parseJsonHex hexText = do
    -- Decode hex to get JSON bytes
    jsonBytes <- case Base16.decode (T.encodeUtf8 hexText) of
        Left err -> Left $ "Hex decode failed: " <> err
        Right bs -> Right bs
    -- Parse JSON array of integers
    case eitherDecodeStrict jsonBytes :: Either String [Word8] of
        Left err -> Left $ "JSON parse failed: " <> err
        Right bytes -> Right $ BS.pack bytes

-- | Parse a verification key from JSON-hex format
parseVerificationKey :: Text -> Either String AncillaryVerificationKey
parseVerificationKey hexText = do
    bs <- parseJsonHex hexText
    case Ed25519.publicKey bs of
        CryptoFailed e -> Left $ "Invalid Ed25519 public key: " <> show e
        CryptoPassed pk -> Right $ AncillaryVerificationKey pk

{- | Compute the manifest hash for signature verification

The hash is computed as SHA256 of concatenated (path || hash) pairs,
sorted by path (BTreeMap order in Rust = lexicographic).
-}
computeManifestHash :: Map FilePath Text -> ByteString
computeManifestHash fileHashes =
    convert
        $ hashFinalize
        $ foldl addEntry (hashInit @SHA256) sortedEntries
  where
    sortedEntries = Map.toAscList fileHashes
    addEntry ctx (path, hash) =
        hashUpdate
            (hashUpdate ctx (T.encodeUtf8 $ T.pack path))
            (T.encodeUtf8 hash)

-- | Verify an Ed25519 signature
verifySignature
    :: AncillaryVerificationKey
    -> ByteString
    -- ^ Message (manifest hash)
    -> ManifestSignature
    -> Bool
verifySignature (AncillaryVerificationKey pk) msg (ManifestSignature sig) =
    Ed25519.verify pk msg sig

-- | Compute SHA256 hash of a file
computeFileHash :: FilePath -> IO ByteString
computeFileHash path = do
    contents <- BS.readFile path
    pure $ convert $ hashFinalize $ hashUpdate (hashInit @SHA256) contents

{- | Verify an ancillary manifest

This function:
1. Reads and parses @ancillary_manifest.json@ from the directory
2. Verifies each file's SHA256 hash matches the manifest
3. Verifies the Ed25519 signature over the manifest hash
-}
verifyAncillaryManifest
    :: AncillaryVerificationKey
    -> FilePath
    -- ^ Directory containing extracted ancillary files
    -> IO (Either AncillaryVerificationError ())
verifyAncillaryManifest vk baseDir = do
    let manifestPath = baseDir <> "/ancillary_manifest.json"

    -- Check manifest exists
    exists <- doesFileExist manifestPath
    if not exists
        then pure $ Left $ ManifestNotFound manifestPath
        else do
            -- Parse manifest
            manifestBytes <- BS.readFile manifestPath
            case eitherDecodeStrict manifestBytes of
                Left err -> pure $ Left $ ManifestParseError manifestPath err
                Right manifest -> verifyManifestContents vk baseDir manifest

-- | Verify manifest contents (file hashes and signature)
verifyManifestContents
    :: AncillaryVerificationKey
    -> FilePath
    -> AncillaryManifest
    -> IO (Either AncillaryVerificationError ())
verifyManifestContents vk baseDir AncillaryManifest{..} = do
    -- Verify file hashes
    hashResult <- verifyFileHashes baseDir manifestData
    case hashResult of
        Left err -> pure $ Left err
        Right () -> do
            -- Verify signature
            case manifestSignature of
                Nothing -> pure $ Left SignatureMissing
                Just sig -> do
                    let manifestHash = computeManifestHash manifestData
                    if verifySignature vk manifestHash sig
                        then pure $ Right ()
                        else pure $ Left SignatureInvalid

-- | Verify all file hashes in the manifest
verifyFileHashes
    :: FilePath
    -> Map FilePath Text
    -> IO (Either AncillaryVerificationError ())
verifyFileHashes baseDir fileHashes = do
    let entries = Map.toList fileHashes
    go entries
  where
    go [] = pure $ Right ()
    go ((relPath, expectedHash) : rest) = do
        let fullPath = baseDir <> "/" <> relPath
        exists <- doesFileExist fullPath
        if not exists
            then pure $ Left $ FileNotFound fullPath
            else do
                actualHash <- computeFileHashHex fullPath
                if actualHash == expectedHash
                    then go rest
                    else
                        pure
                            $ Left
                            $ HashMismatch relPath expectedHash actualHash

    computeFileHashHex path = do
        hash <- computeFileHash path
        pure $ T.decodeUtf8 $ Base16.encode hash
