{- |
Module      : Cardano.UTxOCSMT.Mithril.AncillaryVerifierSpec
Description : Unit tests for Ed25519 ancillary verification

Tests for JSON-hex decoding, key parsing, and manifest hash computation.
-}
module Cardano.UTxOCSMT.Mithril.AncillaryVerifierSpec
    ( spec
    )
where

import Cardano.UTxOCSMT.Mithril.AncillaryVerifier
    ( AncillaryVerificationError (..)
    , AncillaryVerificationKey
    , computeManifestHash
    , parseJsonHex
    , parseVerificationKey
    , verifyAncillaryManifest
    )
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
    ( Spec
    , describe
    , it
    , shouldBe
    , shouldSatisfy
    )

spec :: Spec
spec = describe "AncillaryVerifier" $ do
    describe "parseJsonHex" $ do
        it "decodes simple JSON-hex array" $ do
            -- hex("[1,2,3]") = "5b312c322c335d"
            let hex = "5b312c322c335d"
            parseJsonHex hex `shouldBe` Right (BS.pack [1, 2, 3])

        it "decodes empty array" $ do
            -- hex("[]") = "5b5d"
            let hex = "5b5d"
            parseJsonHex hex `shouldBe` Right BS.empty

        it "fails on invalid hex" $ do
            parseJsonHex "not-hex" `shouldSatisfy` isLeft

        it "fails on invalid JSON" $ do
            -- hex("not json") - valid hex but invalid JSON
            let hex = "6e6f74206a736f6e"
            parseJsonHex hex `shouldSatisfy` isLeft

    describe "parseVerificationKey" $ do
        it "parses Preview network key" $ do
            -- This is the actual Preview ancillary verification key
            let previewKey =
                    "5b3138392c3139322c3231362c3135302c3131342c3231362c323\
                    \3372c3231302c34352c31382c32312c3139362c3230382c323436\
                    \2c3134362c322c3235322c3234332c3235312c3139372c32382c3\
                    \135372c3230342c3134352c33302c31342c3232382c3136382c31\
                    \32392c38332c3133362c33365d"
            parseVerificationKey previewKey `shouldSatisfy` isRight

        it "fails on key with wrong length" $ do
            -- Only 16 bytes instead of 32
            let shortKey =
                    "5b312c322c332c342c352c362c372c382c392c31302c31312c31322c31332c31342c31352c31365d"
            parseVerificationKey shortKey `shouldSatisfy` isLeft

    describe "computeManifestHash" $ do
        it "produces consistent hash for same input" $ do
            let files =
                    Map.fromList
                        [ ("file1.txt", "abc123")
                        , ("file2.txt", "def456")
                        ]
            let hash1 = computeManifestHash files
            let hash2 = computeManifestHash files
            hash1 `shouldBe` hash2

        it "produces different hash for different input" $ do
            let files1 = Map.fromList [("file1.txt", "abc123")]
            let files2 = Map.fromList [("file1.txt", "xyz789")]
            let hash1 = computeManifestHash files1
            let hash2 = computeManifestHash files2
            hash1 `shouldSatisfy` (/= hash2)

        it "hash is 32 bytes (SHA256)" $ do
            let files = Map.fromList [("test", "hash")]
            BS.length (computeManifestHash files) `shouldBe` 32

        it "order is deterministic (sorted by path)" $ do
            -- Same files added in different order should produce same hash
            let files1 =
                    Map.fromList
                        [ ("a.txt", "1")
                        , ("z.txt", "2")
                        , ("m.txt", "3")
                        ]
            let files2 =
                    Map.fromList
                        [ ("z.txt", "2")
                        , ("a.txt", "1")
                        , ("m.txt", "3")
                        ]
            computeManifestHash files1 `shouldBe` computeManifestHash files2

    describe "verifyAncillaryManifest" $ do
        it "returns ManifestNotFound for missing manifest" $ do
            withSystemTempDirectory "verify-test" $ \tmpDir -> do
                result <- verifyAncillaryManifest previewVk tmpDir
                result
                    `shouldBe` Left
                        (ManifestNotFound (tmpDir <> "/ancillary_manifest.json"))

        it "returns ManifestParseError for invalid JSON" $ do
            withSystemTempDirectory "verify-test" $ \tmpDir -> do
                let manifestPath = tmpDir <> "/ancillary_manifest.json"
                BS.writeFile manifestPath "not valid json"
                result <- verifyAncillaryManifest previewVk tmpDir
                case result of
                    Left (ManifestParseError path _) ->
                        path `shouldBe` manifestPath
                    other ->
                        fail $ "Expected ManifestParseError, got: " <> show other

        it "returns SignatureMissing when manifest has no signature" $ do
            withSystemTempDirectory "verify-test" $ \tmpDir -> do
                let manifestPath = tmpDir <> "/ancillary_manifest.json"
                -- Manifest without signature field
                BS.writeFile manifestPath "{\"data\":{}}"
                result <- verifyAncillaryManifest previewVk tmpDir
                result `shouldBe` Left SignatureMissing

-- Helper functions
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Preview network verification key (known valid)
previewVk :: AncillaryVerificationKey
previewVk =
    case parseVerificationKey previewKeyHex of
        Right vk -> vk
        Left err -> error $ "Test setup error: " <> err
  where
    previewKeyHex =
        "5b3138392c3139322c3231362c3135302c3131342c3231362c323\
        \3372c3231302c34352c31382c32312c3139362c3230382c323436\
        \2c3134362c322c3235322c3234332c3235312c3139372c32382c3\
        \135372c3230342c3134352c33302c31342c3232382c3136382c31\
        \32392c38332c3133362c33365d"
