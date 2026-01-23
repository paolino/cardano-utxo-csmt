module Cardano.UTxOCSMT.Application.Run.Base16
    ( encodeBase16Text
    , decodeBase16Text
    , unsafeDecodeBase16Text
    ) where

import Data.ByteArray (ByteArray, ByteArrayAccess)
import Data.ByteArray.Encoding
    ( Base (..)
    , convertFromBase
    , convertToBase
    )
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | Encode a ByteString to Base16 Text
encodeBase16Text :: ByteArrayAccess a => a -> T.Text
encodeBase16Text = TE.decodeUtf8 . convertToBase Base16

-- | Decode a Base16 Text to ByteString
decodeBase16Text :: ByteArray a => T.Text -> Either String a
decodeBase16Text = convertFromBase Base16 . TE.encodeUtf8

unsafeDecodeBase16Text :: ByteArray a => T.Text -> a
unsafeDecodeBase16Text txt =
    case decodeBase16Text txt of
        Left err -> error $ "unsafeDecodeBase16Text: invalid base16: " ++ err
        Right bs -> bs
