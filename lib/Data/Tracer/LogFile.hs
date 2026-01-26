module Data.Tracer.LogFile
    ( logFileTracer
    , logTracer
    ) where

import Control.Tracer (Tracer (..))
import System.IO
    ( BufferMode (..)
    , IOMode (..)
    , hPutStrLn
    , hSetBuffering
    , stdout
    , withFile
    )

logFileTracer :: FilePath -> (Tracer IO String -> IO a) -> IO a
logFileTracer fp k = do
    withFile fp AppendMode $ \handle -> do
        hSetBuffering handle LineBuffering
        k $ Tracer $ \msg -> hPutStrLn handle msg

{- | Create a tracer that logs to a file if a filepath is provided,
otherwise logs to stdout with line buffering.
-}
logTracer :: Maybe FilePath -> (Tracer IO String -> IO a) -> IO a
logTracer Nothing k = do
    hSetBuffering stdout LineBuffering
    k $ Tracer $ \msg -> hPutStrLn stdout msg
logTracer (Just fp) k = logFileTracer fp k
