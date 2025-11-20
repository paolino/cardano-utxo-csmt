module Cardano.UTxO.CSMT.App
    ( main
    )
where

import CSMT.Backend.RocksDB
    ( RunRocksDB (RunRocksDB)
    , rocksDBBackend
    , withRocksDB
    )
import CSMT.Hashes (insert, mkHash)
import Codec.CBOR.Decoding
    ( Decoder
    , decodeBreakOr
    , decodeMapLenIndef
    )
import Codec.CBOR.Read
    ( DeserialiseFailure (..)
    , IDecode (..)
    , deserialiseIncremental
    )
import Codec.CBOR.Term (Term (..), decodeTerm, encodeTerm)
import Codec.CBOR.Write (toStrictByteString)
import Control.Monad.ST (RealWorld, stToIO)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor (($>))
import Database.RocksDB (Config (..), DB, put, withDB)
import OptEnvConf (Parser, runParser, setting)
import OptEnvConf.Setting
    ( auto
    , help
    , long
    , metavar
    , option
    , reader
    , short
    , str
    , value
    )
import Paths_cardano_utxo_csmt (version)
import Streaming (MonadIO (..), Of, Stream, effect)
import Streaming.ByteString qualified as SB
import Streaming.Prelude qualified as S
import System.IO (IOMode (..), withFile)

data Option = Option
    { optInputFile :: FilePath
    , optCSMTPath :: FilePath
    , optKVDBPath :: Maybe FilePath
    , optMaxEntries :: Maybe Int
    }

optInputFileParser :: Parser FilePath
optInputFileParser =
    setting
        [ short 'i'
        , long "input-file"
        , help "Path to the input UTxO Cbor file"
        , metavar "FILE"
        , reader str
        , option
        ]

optCSMTPathParser :: Parser FilePath
optCSMTPathParser =
    setting
        [ short 'c'
        , long "csmt-db-path"
        , help "Path to the CSMT RocksDB database"
        , metavar "DIR"
        , reader str
        , option
        ]

optKVDBPathParser :: Parser (Maybe FilePath)
optKVDBPathParser =
    setting
        [ short 'k'
        , long "kvdb-path"
        , help "Path to the KVDB database (optional)"
        , metavar "DIR"
        , reader (Just <$> str)
        , option
        , value Nothing
        ]

optMaxEntriesParser :: Parser (Maybe Int)
optMaxEntriesParser =
    setting
        [ short 'n'
        , long "max-entries"
        , help "Maximum number of entries to process"
        , metavar "INT"
        , reader (Just <$> auto)
        , option
        , value Nothing
        ]

main :: IO ()
main = do
    options <-
        runParser version "Cardano UTxO CSMT Importer"
            $ Option
                <$> optInputFileParser
                <*> optCSMTPathParser
                <*> optKVDBPathParser
                <*> optMaxEntriesParser
    readUTxOCBORIncremental options

kvConfig :: Config
kvConfig =
    Config
        { createIfMissing = True
        , errorIfExists = False
        , paranoidChecks = False
        , maxFiles = Nothing
        , prefixLength = Nothing
        , bloomFilter = False
        }

readUTxOCBORIncremental :: Option -> IO ()
readUTxOCBORIncremental
    Option
        { optInputFile
        , optCSMTPath
        , optKVDBPath
        , optMaxEntries
        } =
        withOptionalDB $ \mKVDB -> do
            withRocksDB optCSMTPath $ \run -> do
                withFile optInputFile ReadMode $ \handle ->
                    SB.hGetContents handle
                        & SB.toChunks
                        & start
                        & elements
                        & part
                        & insertUTxO run mKVDB
      where
        withOptionalDB f = case optKVDBPath of
            Just path -> withDB path kvConfig $ f . Just
            Nothing -> f Nothing
        part = maybe ($> ()) S.take optMaxEntries

insertUTxO
    :: RunRocksDB -> Maybe DB -> Stream (Of (Term, Term)) IO () -> IO ()
insertUTxO (RunRocksDB run) mDB = S.mapM_ $ \(txin, term) -> do
    let key = toStrictByteString $ encodeTerm txin
        value' = toStrictByteString $ encodeTerm term
    run $ insert (rocksDBBackend mkHash) key value'
    case mDB of
        Just db -> put db key value'
        Nothing -> return ()

start
    :: Stream (Of ByteString) IO r
    -> Stream (Of ByteString) IO (Either DeserialiseFailure r)
start = parse Right d decodeMapLenIndef
  where
    d _ s r _ () = do
        S.yield r
        Right <$> s

elements
    :: Stream (Of ByteString) IO (Either DeserialiseFailure r)
    -> Stream (Of (Term, Term)) IO (Either DeserialiseFailure (Either () r))
elements = parse (fmap Right) d $ do
    b <- decodeBreakOr
    if b
        then return Nothing
        else fmap Just . (,) <$> decodeTerm <*> decodeTerm
  where
    d p s r go = \case
        Nothing -> return $ Right $ Left ()
        (Just v) -> do
            S.yield v
            go (S.yield r >> s) p

type ParseFailed = Either DeserialiseFailure

parse
    :: MonadIO m
    => (a -> ParseFailed b)
    -> ( IDecode RealWorld t
         -> Stream (Of ByteString) m a
         -> ByteString
         -> ( Stream (Of ByteString) m a
              -> IDecode RealWorld t
              -> Stream (Of x) m (ParseFailed b)
            )
         -> t
         -> Stream (Of x) m (ParseFailed b)
       )
    -> Decoder RealWorld t
    -> Stream (Of ByteString) m a
    -> Stream (Of x) m (ParseFailed b)
parse f d p0 s0 = do
    p1 <- liftIO $ stToIO $ deserialiseIncremental p0
    let go s p = case p of
            Done rest _ r -> d p1 s rest go r
            Fail _ _ err -> return $ Left err
            Partial k -> effect $ do
                mp <- S.next s
                return $ case mp of
                    Left r -> return $ f r
                    Right (chunk, rest) -> do
                        p' <- liftIO $ stToIO $ k (Just chunk)
                        go rest p'
    go s0 p1
