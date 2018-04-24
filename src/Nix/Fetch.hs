{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Fetch where

import Control.Monad.IO.Class
import Data.Semigroup ((<>))
import Data.ByteString (ByteString)
import qualified Codec.Archive.Tar as Tar
import Nix.Value
import Data.IORef
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Pipes.GZip as PG
import qualified Pipes.Zlib as PZ
import qualified Data.ByteString.Base16 as B16
import  System.Directory
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Data.Bits
import GHC.Word
import qualified Data.ByteString as B8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Char as Char

import Control.Monad.State
import qualified Data.ByteString.Builder as Builder
import Pipes
import Control.Monad
import Pipes.Prelude (fold, fold', generalize, scan, tee)
import qualified Pipes.Prelude as PP
import qualified Pipes.ByteString as PB
import qualified Pipes.HTTP as HTTP
import Network.HTTP.Client.TLS
import qualified Crypto.Hash.SHA256 as Crypto
import System.Random
import System.IO (IOMode(..), withFile)

fetch :: Text -> Maybe Text -> IO FilePath
fetch uri msha = do
    fn <- tmpFilepath
    mgr <- HTTP.newManager tlsManagerSettings
    sha <- newIORef Crypto.init
    req <- HTTP.parseUrl (Text.unpack uri)

    tarBytes <- HTTP.withHTTP req mgr $ \resp ->
        PB.toLazyM $
        PG.decompress
        (for (HTTP.responseBody resp) $ \b -> do
                lift (modifyIORef sha (flip Crypto.update b))
                yield b)
    BS.putStrLn . ("sha: " <>) . Crypto.finalize =<< readIORef sha
    Tar.unpack fn $ Tar.read tarBytes
    return fn

tmpFilepath :: IO FilePath
tmpFilepath = do
    r ::  Integer <- randomRIO (1,1000000000)
    b <- getTemporaryDirectory
    return $ b </> "hnix_tmp_" <> show r

-- TODO
decode32 :: ByteString -> Either String Text
decode32 = undefined
    where
  -- a-z or 2-7
  is32 :: Word8 -> Bool
  is32 w = (w >= 97 && w <= 122) || (w >= 50 && w <= 55)