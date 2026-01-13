module Cardano.UTxO.CSMT.App
    ( main
    )
where

import CSMT
    ( Standalone (StandaloneCSMTCol, StandaloneKVCol)
    , StandaloneCodecs (..)
    )
import CSMT.Backend.RocksDB
    ( RunRocksDB (RunRocksDB)
    , standaloneRocksDBDatabase
    , withRocksDB
    )
import CSMT.Hashes (Hash, fromKVHashes, insert, isoHash)
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
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Database.KV.Transaction qualified as Transaction
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
                <*> optMaxEntriesParser
    readUTxOCBORIncremental options

readUTxOCBORIncremental :: Option -> IO ()
readUTxOCBORIncremental
    Option
        { optInputFile
        , optCSMTPath
        , optMaxEntries
        } =
        withRocksDB optCSMTPath 2 1 $ \run -> do
            withFile optInputFile ReadMode $ \handle ->
                SB.hGetContents handle
                    & SB.toChunks
                    & start
                    & elements
                    & reportEvery 10000
                    & part
                    & insertUTxO run
      where
        part = maybe ($> ()) S.take optMaxEntries

codecs :: StandaloneCodecs ByteString ByteString Hash
codecs =
    StandaloneCodecs
        { keyCodec = id
        , valueCodec = id
        , nodeCodec = isoHash
        }

insertUTxO
    :: RunRocksDB -> Stream (Of (Term, Term)) IO () -> IO ()
insertUTxO (RunRocksDB run) = S.mapM_ $ \(txin, term) -> do
    let key = toStrictByteString $ encodeTerm txin
        value' = toStrictByteString $ encodeTerm term
    run $ do
        database <- standaloneRocksDBDatabase codecs
        Transaction.run database
            $ insert fromKVHashes StandaloneKVCol StandaloneCSMTCol key value'

reportEvery :: Int -> Stream (Of a) IO b -> Stream (Of a) IO b
reportEvery n s0 = do
    now <- liftIO getCurrentTime
    go 0 now s0
  where
    go :: Int -> UTCTime -> Stream (Of a) IO b -> Stream (Of a) IO b
    go i pt s = effect $ do
        mp <- S.next s
        pure $ case mp of
            Left r -> return r
            Right (x, rest) -> do
                nt <-
                    liftIO
                        $ if (i + 1) `mod` n == 0
                            then do
                                ct <- getCurrentTime
                                let diff = ct `diffUTCTime` pt
                                putStrLn
                                    $ "Processed "
                                        <> show (i + 1)
                                        <> " UTxOs ("
                                        <> show @Double
                                            (fromIntegral n / realToFrac diff)
                                        <> " UTxOs/second)"
                                pure ct
                            else pure pt

                S.yield x
                go (i + 1) nt rest
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
