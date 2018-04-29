{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Nix.Fetch where

import Control.Monad.IO.Class
import Data.Semigroup ((<>))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Debug.Trace
import qualified Codec.Archive.Tar as Tar
import Data.Maybe (catMaybes)
import qualified Codec.Archive.Tar.Entry as Tar
import Data.List (intersperse, intercalate)
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
import System.FilePath
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
import qualified Crypto.Hash.Algorithms as Crypto
import qualified Crypto.Hash as Crypto
import System.Random
import System.IO (IOMode(..), withFile)
import Nix.Hash


fetchToTmp :: Text -> Maybe Text -> IO FilePath
fetchToTmp uri msha = do
    fn <- tmpFilepath
    mgr <- HTTP.newManager tlsManagerSettings
    sha <- newIORef Crypto.hashInit
    req <- HTTP.parseUrl (Text.unpack uri)

    tarBytes <- HTTP.withHTTP req mgr $ \resp ->
        PB.toLazyM $
        PG.decompress
        (for (HTTP.responseBody resp) $ \b -> do
                lift (modifyIORef sha (flip Crypto.hashUpdate b))
                yield b)
    BS.putStrLn . ("sha: " <>) . sanitizeDigest32 . shaToDigest32 . Crypto.hashFinalize =<< readIORef sha
    Tar.unpack fn $ stripComponents $ Tar.read tarBytes
    -- Tar.unpack fn $ Tar.read tarBytes
    return fn


stripComponents :: Tar.Entries Tar.FormatError -> Tar.Entries Tar.FormatError
stripComponents =
    fmapMaybeEntries (stripEntry 1)
    -- fmap (either id (const Tar.UnrecognisedTarFormat))
    -- . Tar.mapEntries (stripEntry 1)

fmapMaybeEntries :: Show e => (Tar.Entry -> Maybe Tar.Entry) -> Tar.Entries e -> Tar.Entries e
fmapMaybeEntries f =
    Tar.unfoldEntries (\case
                          [] -> Right Nothing
                          (e:xs) -> Right (Just (e,xs))
                      )
    . catMaybes
    . fmap f
    . Tar.foldEntries (:) [] (error . show)

stripEntry :: Int -> Tar.Entry -> Maybe Tar.Entry
stripEntry n e = e'
    where
      isDir = Tar.entryContent e == Tar.Directory
      parts = drop 1 $ splitPath $ Tar.entryPath e
      e' = case parts of
          [] -> Nothing
          _  -> let p' = Tar.toTarPath isDir (joinPath parts)
                in  either (const Nothing) (\p'' -> Just $ e { Tar.entryTarPath = p'' }) p'

      -- setPath :: FilePath -> Either String Tar.Entry
      -- setPath fp = fmap (\fp' -> e { Tar.entryTarPath = fp' })
      --                   (Tar.toTarPath isDir fp)
      -- e'  = case Tar.fromTarPath (Tar.entryTarPath e) of
      --     -- (pathSeparator : _) -> error (show e) -- Left "Absolute path in tar entry"
      --     _ ->
      --         setPath $ joinPath
      --                 $ drop n
      --                 $ splitPath
      --                 $ Tar.fromTarPath
      --                 $ Tar.entryTarPath e


tmpFilepath :: IO FilePath
tmpFilepath = do
    r ::  Integer <- randomRIO (1,1000000000)
    b <- getTemporaryDirectory
    return $ b </> "hnix_tmp_" <> show r </> "source"

-- TODO
decode32 :: ByteString -> Either String Text
decode32 = undefined
    where
  -- a-z or 2-7
  is32 :: Word8 -> Bool
  is32 w = (w >= 97 && w <= 122) || (w >= 50 && w <= 55)


-- fetch_ :: Text -> IO [Tar.Entry]
-- fetch_ uri = do
--     mgr <- HTTP.newManager tlsManagerSettings
--     sha <- newIORef Crypto.init
--     req <- HTTP.parseUrl (Text.unpack uri)
--     tarBytes <- HTTP.withHTTP req mgr $ \resp ->
--         PB.toLazyM $ PG.decompress
--         (for (HTTP.responseBody resp) $ \b -> do
--                 lift (modifyIORef sha (flip Crypto.update b))
--                 yield b)
--     return . Tar.foldEntries (:) [] (const [])
--            . stripComponents
--            . Tar.read
--            $ tarBytes

-- fetch' :: Text -> IO (Tar.Entries Tar.FormatError)
-- fetch' uri = do
--     mgr <- HTTP.newManager tlsManagerSettings
--     sha <- newIORef Crypto.init
--     req <- HTTP.parseUrl (Text.unpack uri)
--     tarBytes <- HTTP.withHTTP req mgr $ \resp ->
--         PB.toLazyM $ PG.decompress
--         (for (HTTP.responseBody resp) $ \b -> do
--                 lift (modifyIORef sha (flip Crypto.update b))
--                 yield b)
--     return $ Tar.read tarBytes
--     -- return . Tar.foldEntries (:) [] (const [])
--     --        . stripComponents
--     --        . Tar.read
--     --        $ tarBytes


