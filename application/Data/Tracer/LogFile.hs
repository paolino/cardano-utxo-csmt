module Data.Tracer.LogFile
    ( logFileTracer
    , logTracer
    ) where

import Control.Tracer (Tracer (..), stdoutTracer)
import System.IO
    ( BufferMode (..)
    , IOMode (..)
    , hPutStrLn
    , hSetBuffering
    , withFile
    )

logFileTracer :: FilePath -> (Tracer IO String -> IO a) -> IO a
logFileTracer fp k = do
    withFile fp AppendMode $ \handle -> do
        hSetBuffering handle LineBuffering
        k $ Tracer $ \msg -> hPutStrLn handle msg

-- | Create a tracer that logs to a file if a filepath is provided,
-- otherwise logs to stdout.
logTracer :: Maybe FilePath -> (Tracer IO String -> IO a) -> IO a
logTracer Nothing k = k stdoutTracer
logTracer (Just fp) k = logFileTracer fp k
