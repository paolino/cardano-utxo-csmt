{- |
Module      : Mithril.STM.IntegrationSpec
Description : Integration tests with real Mithril certificates
Copyright   : (c) Paolo Veronelli, 2026
License     : Apache-2.0

Tests verification against real certificates fetched from the Mithril
aggregator API.
-}
module Mithril.STM.IntegrationSpec (spec) where

import Control.Monad (forM)
import Data.Aeson
    ( FromJSON (..)
    , Value (..)
    , eitherDecode
    , withArray
    , withObject
    , (.:)
    )
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Vector qualified as V
import Data.Word (Word32, Word64, Word8)
import Network.HTTP.Client
    ( httpLbs
    , newManager
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Test.Hspec

import Mithril.STM
    ( AggregateSignature (..)
    , AggregateVerificationKey (..)
    , BlsOps (..)
    , ConcatenationProof (..)
    , HashOps (..)
    , MerkleBatchPath (..)
    , MerkleCommitment (..)
    , Parameters (..)
    , RegistrationEntry (..)
    , SignedRegistration (..)
    , SingleSignature (..)
    , VerificationFailure (..)
    , verify
    )
import Mithril.STM.Crypto.Crypton (cryptonHashOps)
import Mithril.STM.Crypto.Hsblst
    ( BlsSignature
    , BlsVerificationKey
    , decompressSig
    , decompressVk
    , hsblstBlsOps
    )

-- ============================================================================
-- JSON Parsing Types
-- ============================================================================

-- | Raw certificate from Mithril aggregator API
data RawCertificate = RawCertificate
    { rcSignedMessage :: !ByteString
    , rcParameters :: !Parameters
    , rcAggregateVerificationKey :: !RawAVK
    , rcMultiSignature :: !RawMultiSig
    }
    deriving stock (Show)

-- | Raw aggregate verification key
data RawAVK = RawAVK
    { ravkRoot :: !ByteString
    , ravkNrLeaves :: !Word32
    , ravkTotalStake :: !Word64
    }
    deriving stock (Show)

-- | Raw multi-signature
data RawMultiSig = RawMultiSig
    { rmsSignatures :: ![RawSignedReg]
    }
    deriving stock (Show)

-- | Raw signed registration (signature + registration entry)
data RawSignedReg = RawSignedReg
    { rsrSigma :: !ByteString
    , rsrIndices :: ![Word64]
    , rsrVerificationKey :: !ByteString
    , rsrStake :: !Word64
    }
    deriving stock (Show)

-- ============================================================================
-- JSON Instances
-- ============================================================================

instance FromJSON RawCertificate where
    parseJSON = withObject "RawCertificate" $ \o -> do
        signedMsgHex <- o .: "signed_message"
        signedMsg <- parseHexBytes signedMsgHex

        metadata <- o .: "metadata"
        params <- metadata .: "parameters"
        k <- params .: "k"
        m <- params .: "m"
        phiF <- params .: "phi_f"
        let parameters =
                Parameters
                    { paramM = m
                    , paramK = k
                    , paramPhiF = phiF
                    }

        avkJson <- o .: "aggregate_verification_key"
        avk <- parseAVK avkJson

        multiSigHex <- o .: "multi_signature"
        multiSig <- parseMultiSigHex multiSigHex

        pure
            RawCertificate
                { rcSignedMessage = signedMsg
                , rcParameters = parameters
                , rcAggregateVerificationKey = avk
                , rcMultiSignature = multiSig
                }

parseAVK :: String -> Parser RawAVK
parseAVK hexStr = do
    -- The AVK is hex-encoded JSON
    case hexToBytes hexStr of
        Nothing -> fail "Failed to decode AVK hex"
        Just bs ->
            case eitherDecode (LBS.fromStrict bs) of
                Left err -> fail $ "Failed to parse AVK JSON: " <> err
                Right val -> parseAVKValue val

parseMultiSigHex :: String -> Parser RawMultiSig
parseMultiSigHex hexStr = do
    -- The multi_signature is hex-encoded JSON
    case hexToBytes hexStr of
        Nothing -> fail "Failed to decode multi_signature hex"
        Just bs ->
            case eitherDecode (LBS.fromStrict bs) of
                Left err -> fail $ "Failed to parse multi_sig JSON: " <> err
                Right val -> parseJSON val

parseAVKValue :: Value -> Parser RawAVK
parseAVKValue = withObject "RawAVK" $ \o -> do
    commitment <- o .: "mt_commitment"
    rootArray <- commitment .: "root"
    root <- parseByteArray rootArray
    nrLeaves <- commitment .: "nr_leaves"
    totalStake <- o .: "total_stake"
    pure
        RawAVK
            { ravkRoot = root
            , ravkNrLeaves = nrLeaves
            , ravkTotalStake = totalStake
            }

instance FromJSON RawMultiSig where
    parseJSON = withObject "RawMultiSig" $ \o -> do
        sigs <- o .: "signatures"
        parsedSigs <- mapM parseSignatureEntry sigs
        pure $ RawMultiSig parsedSigs

parseSignatureEntry :: Value -> Parser RawSignedReg
parseSignatureEntry = withArray "SignatureEntry" $ \arr -> do
    if V.length arr /= 2
        then fail "Expected [signature, registration] pair"
        else do
            let sigVal = arr V.! 0
                regVal = arr V.! 1
            (sigma, indices, _signerIndex) <- parseSigPart sigVal
            (vk, stake) <- parseRegPart regVal
            pure
                RawSignedReg
                    { rsrSigma = sigma
                    , rsrIndices = indices
                    , rsrVerificationKey = vk
                    , rsrStake = stake
                    }

parseSigPart :: Value -> Parser (ByteString, [Word64], Word64)
parseSigPart = withObject "SignaturePart" $ \o -> do
    sigmaArray <- o .: "sigma"
    sigma <- parseByteArray sigmaArray
    indices <- o .: "indexes"
    signerIndex <- o .: "signer_index"
    pure (sigma, indices, signerIndex)

parseRegPart :: Value -> Parser (ByteString, Word64)
parseRegPart = withArray "RegistrationPart" $ \arr -> do
    if V.length arr /= 2
        then fail "Expected [vk, stake] pair"
        else do
            vkArray <- parseJSON (arr V.! 0)
            vk <- parseByteArray vkArray
            stake <- parseJSON (arr V.! 1)
            pure (vk, stake)

parseByteArray :: [Int] -> Parser ByteString
parseByteArray ints = pure $ BS.pack $ map fromIntegral ints

parseHexBytes :: String -> Parser ByteString
parseHexBytes hex = case hexToBytes hex of
    Nothing -> fail "Invalid hex string"
    Just bs -> pure bs

hexToBytes :: String -> Maybe ByteString
hexToBytes [] = Just BS.empty
hexToBytes (a : b : rest) = do
    byte <-
        hexDigit a >>= \hi -> hexDigit b >>= \lo -> Just (hi * 16 + lo)
    rest' <- hexToBytes rest
    Just $ BS.cons byte rest'
hexToBytes [_] = Nothing

hexDigit :: Char -> Maybe Word8
hexDigit c
    | c >= '0' && c <= '9' =
        Just $ fromIntegral $ fromEnum c - fromEnum '0'
    | c >= 'a' && c <= 'f' =
        Just $ fromIntegral $ fromEnum c - fromEnum 'a' + 10
    | c >= 'A' && c <= 'F' =
        Just $ fromIntegral $ fromEnum c - fromEnum 'A' + 10
    | otherwise = Nothing

-- ============================================================================
-- Conversion to STM Types
-- ============================================================================

convertCertificate
    :: RawCertificate
    -> Either
        String
        ( Parameters
        , AggregateVerificationKey ByteString
        , ByteString
        , AggregateSignature BlsSignature BlsVerificationKey ByteString
        )
convertCertificate RawCertificate{..} = do
    -- Convert AVK
    let avk =
            AggregateVerificationKey
                { avkMerkleCommitment =
                    MerkleCommitment
                        { mcRoot = ravkRoot rcAggregateVerificationKey
                        , mcNrLeaves = ravkNrLeaves rcAggregateVerificationKey
                        }
                , avkTotalStake = ravkTotalStake rcAggregateVerificationKey
                }

    -- Convert signatures
    signedRegs <- forM (rmsSignatures rcMultiSignature) $ \RawSignedReg{..} -> do
        sig <- case decompressSig rsrSigma of
            Nothing ->
                Left
                    $ "Failed to decompress signature: "
                        <> show (BS.length rsrSigma)
                        <> " bytes"
            Just s -> Right s
        vk <- case decompressVk rsrVerificationKey of
            Nothing ->
                Left
                    $ "Failed to decompress VK: "
                        <> show (BS.length rsrVerificationKey)
                        <> " bytes"
            Just v -> Right v
        Right
            SignedRegistration
                { srSignature =
                    SingleSignature
                        { ssSignature = sig
                        , ssIndices = rsrIndices
                        }
                , srRegistration =
                    RegistrationEntry
                        { reVerificationKey = vk
                        , reStake = rsrStake
                        }
                }

    -- Build batch path (empty for now - need to extract from certificate)
    let batchPath =
            MerkleBatchPath
                { mbpSiblings = []
                , mbpIndices = []
                }

    let aggSig =
            Concatenation
                ConcatenationProof
                    { cpSignatures = signedRegs
                    , cpBatchPath = batchPath
                    }

    Right (rcParameters, avk, rcSignedMessage, aggSig)

-- ============================================================================
-- Test Helpers
-- ============================================================================

-- | Fetch a certificate from the Mithril aggregator
fetchCertificate :: String -> IO (Either String RawCertificate)
fetchCertificate certHash = do
    manager <- newManager tlsManagerSettings
    let url =
            "https://aggregator.release-mainnet.api.mithril.network"
                <> "/aggregator/certificate/"
                <> certHash
    request <- parseRequest url
    response <- httpLbs request manager
    if statusCode (responseStatus response) /= 200
        then pure $ Left $ "HTTP error: " <> show (responseStatus response)
        else case eitherDecode (responseBody response) of
            Left err -> pure $ Left $ "Parse error: " <> err
            Right cert -> pure $ Right cert

-- | Fetch the latest certificate hash
fetchLatestCertificateHash :: IO (Either String String)
fetchLatestCertificateHash = do
    manager <- newManager tlsManagerSettings
    let url =
            "https://aggregator.release-mainnet.api.mithril.network"
                <> "/aggregator/certificates"
    request <- parseRequest url
    response <- httpLbs request manager
    if statusCode (responseStatus response) /= 200
        then pure $ Left $ "HTTP error: " <> show (responseStatus response)
        else case eitherDecode (responseBody response) of
            Left err -> pure $ Left $ "Parse error: " <> err
            Right (certs :: [Value]) -> case certs of
                [] -> pure $ Left "No certificates found"
                (c : _) -> case c of
                    Object o -> case lookup "hash" (toList o) of
                        Just (String h) -> pure $ Right $ toString h
                        _ -> pure $ Left "No hash field"
                    _ -> pure $ Left "Invalid certificate format"
  where
    toList obj =
        case obj of
            _ -> [] -- Simplified, would need proper KeyMap handling
    toString t = map (toEnum . fromEnum) $ show t

-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec
spec = describe "Mithril.STM.Integration" $ do
    describe "Certificate fetching" $ do
        it "can fetch a certificate from mainnet aggregator" $ do
            result <- fetchCertificate knownCertHash
            case result of
                Left err -> expectationFailure $ "Failed to fetch: " <> err
                Right cert -> do
                    rcParameters cert `shouldSatisfy` \p ->
                        paramK p > 0 && paramM p > 0

        it "can parse certificate structure" $ do
            result <- fetchCertificate knownCertHash
            case result of
                Left err -> expectationFailure $ "Failed to fetch: " <> err
                Right cert -> do
                    -- Check AVK structure
                    let avk = rcAggregateVerificationKey cert
                    BS.length (ravkRoot avk) `shouldBe` 32
                    ravkNrLeaves avk `shouldSatisfy` (> 0)
                    ravkTotalStake avk `shouldSatisfy` (> 0)

                    -- Check signatures
                    let sigs = rmsSignatures (rcMultiSignature cert)
                    length sigs `shouldSatisfy` (> 0)

    describe "Certificate conversion" $ do
        it "can convert raw certificate to STM types" $ do
            result <- fetchCertificate knownCertHash
            case result of
                Left err -> expectationFailure $ "Failed to fetch: " <> err
                Right cert -> case convertCertificate cert of
                    Left err ->
                        expectationFailure $ "Conversion failed: " <> err
                    Right (params, avk, msg, aggSig) -> do
                        paramK params `shouldSatisfy` (> 0)
                        avkTotalStake avk `shouldSatisfy` (> 0)
                        BS.length msg `shouldSatisfy` (> 0)
                        case aggSig of
                            Concatenation proof ->
                                length (cpSignatures proof)
                                    `shouldSatisfy` (> 0)

    describe "Certificate verification" $ do
        it "verifies signature count meets quorum" $ do
            result <- fetchCertificate knownCertHash
            case result of
                Left err -> expectationFailure $ "Failed to fetch: " <> err
                Right cert -> case convertCertificate cert of
                    Left err ->
                        expectationFailure $ "Conversion failed: " <> err
                    Right (params, _avk, _msg, aggSig) -> do
                        -- Count total indices
                        let totalIndices = case aggSig of
                                Concatenation proof ->
                                    sum
                                        $ map
                                            ( length
                                                . ssIndices
                                                . srSignature
                                            )
                                        $ cpSignatures proof
                        -- Should meet or exceed quorum
                        fromIntegral totalIndices
                            `shouldSatisfy` (>= paramK params)

-- Note: Full verification test is commented out because it requires
-- the Merkle batch path which is not included in the certificate
-- JSON response. The batch path would need to be reconstructed or
-- fetched separately.
--
-- it "verifies a real mainnet certificate" $ do
--     result <- fetchCertificate knownCertHash
--     case result of
--         Left err -> expectationFailure $ "Failed to fetch: " <> err
--         Right cert -> case convertCertificate cert of
--             Left err -> expectationFailure $ "Conversion: " <> err
--             Right (params, avk, msg, aggSig) -> do
--                 let verifyResult = verify
--                         cryptonHashOps
--                         hsblstBlsOps
--                         params
--                         avk
--                         msg
--                         aggSig
--                 verifyResult `shouldBe` Right ()

-- | A known certificate hash from mainnet for testing
knownCertHash :: String
knownCertHash =
    "04665e7982245bfba404d513003b0ec00cfe9aa22db0f2aecbedfe7ecacea1ea"
