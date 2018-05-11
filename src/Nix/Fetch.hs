{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PackageImports      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Nix.Fetch where

import qualified Codec.Archive.Tar                   as Tar
import qualified Codec.Archive.Tar.Entry             as Tar
import           Control.Monad.IO.Class
import           Data.Bifunctor                      (first)
import qualified Data.Binary                         as B
import           Data.Bits
import           Data.ByteString                     (ByteString)
import qualified Data.ByteString                     as B8
import qualified Data.ByteString.Base16              as B16
import qualified Data.ByteString.Char8               as BS
import qualified Data.ByteString.Lazy                as BSL
import qualified Data.Char                           as Char
import           Data.IORef
import           Data.List                           (foldl', intercalate,
                                                      intersperse)
import           Data.Maybe                          (catMaybes, fromJust)
import           Data.Semigroup                      ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Debug.Trace
import           GHC.Word
import           Nix.Value
import qualified Pipes.GZip                          as PG
import qualified Pipes.Zlib                          as PZ
import           System.Directory
import           System.FilePath
import           System.FilePath

import           Control.Monad
import           Control.Monad.State
import qualified "cryptonite" Crypto.Hash            as Crypto
import qualified "cryptonite" Crypto.Hash.Algorithms as Crypto
import qualified Data.ByteString.Builder             as Builder
import           Network.HTTP.Client.TLS
import           Nix.Hash
import           Pipes
import qualified Pipes.ByteString                    as PB
import qualified Pipes.HTTP                          as HTTP
import           Pipes.Prelude                       (fold, fold', generalize,
                                                      scan, tee)
import qualified Pipes.Prelude                       as PP
import           System.IO                           (IOMode (..), withFile)
import           System.Random

import           System.Nix.Nar
import           System.Nix.Path


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
    localPackNar narEffectsIO fn >>= BSL.writeFile "source.nar" . B.encode
    localPackNar narEffectsIO fn >>= localUnpackNar narEffectsIO "tmp-source"

    -- This doesn't match what `nix repl` generates..... :(
    narsha <- sanitizeDigest32 . shaToDigest32 . Crypto.hashFinalize . (foldr (flip Crypto.hashUpdate) (Crypto.hashInit)) . BSL.toChunks . B.encode <$> localPackNar narEffectsIO fn
    -- narsha <- sanitizeDigest32 . shaToDigest32 . fromJust . Crypto.digestFromByteString @Crypto.SHA256 @BSL.ByteString . B.encode <$> localPackNar narEffectsIO fn
    -- narsha <- sanitizeDigest32 . shaToDigest32 . _ . BSL.toChunks <$> localPackNar narEffectsIO fn
    BS.putStrLn $ "sha try 2: " <> narsha
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


