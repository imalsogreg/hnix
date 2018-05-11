{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}

module Nix.Hash (
    shaToDigest32
  , sanitizeDigest32
  , Truncated(..)
  ) where

-- import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS
import qualified "cryptonite" Crypto.Hash as Cry
-- import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as E

import Control.Monad (void)
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import Data.Word (Word8)
import GHC.TypeLits (Nat, KnownNat, natVal, type (<=))
import "cryptonite" Crypto.Hash (Digest)
import Crypto.Hash.IO (HashAlgorithm(..),)
import Data.ByteArray (alloc)
import Foreign.Ptr (castPtr, Ptr)
import Foreign.Marshal.Utils (copyBytes)
-- #if MIN_VERSION_cryptonite(0,25,0)
-- import Basement.Block.Mutable (Block)
-- #else
import Foundation.Array (UArray)
-- #endif

-- | Hash algorithm 'algo' truncated to 'size' bytes.
newtype Truncated algo (size :: Nat) = Truncated algo

-- -- | The underlying type of a 'Digest'.
-- #if MIN_VERSION_cryptonite(0,25,0)
-- type DigestUnwrapped = Block Word8
-- #else
type DigestUnwrapped = UArray Word8
-- #endif

-- | Use the 'HashAlgorithm' instance of 'algo' and truncate the final
-- digest.
--
-- The implementation of finalization does some pointer munging that
-- relies on the representational equivalence of a 'Digest' and
-- 'DigestUnwrapped', but there is no way for that to be enforced by
-- the type system. Until/unless cryptonite exports this, we will have
-- to be vigilant to changes in the type.
instance ( HashAlgorithm algo, KnownNat (HashDigestSize algo)
         , KnownNat size, size <= HashDigestSize algo
         ) => HashAlgorithm (Truncated algo size) where
  type HashBlockSize (Truncated algo size) = HashBlockSize algo
  type HashDigestSize (Truncated algo size) = size
  type HashInternalContextSize (Truncated algo size) =
    HashInternalContextSize algo
  hashBlockSize = hashBlockSize @algo . coerce
  hashDigestSize _ = fromIntegral $ natVal @size Proxy
  hashInternalContextSize = hashInternalContextSize @algo . coerce
  hashInternalInit = hashInternalInit @algo . coerce
  hashInternalUpdate = hashInternalUpdate @algo . coerce
  hashInternalFinalize cptr dptr = void @_ @DigestUnwrapped $
      alloc (fromIntegral $ natVal @(HashDigestSize algo) Proxy) go
    where
      go :: Ptr (Digest algo) -> IO ()
      go p = do
        hashInternalFinalize (coerce cptr) p
        copyBytes dptr (castPtr p) (fromIntegral $ natVal @size Proxy)

shaToDigest32 :: Cry.Digest Cry.SHA256 -> BS.ByteString
shaToDigest32 bs =
    E.convertToBase E.Base32
    $ (Cry.hash bs :: Cry.Digest (Truncated Cry.SHA256 20))

-- map from Wikipedia Base32 listing to nix libutil/hash.cc
sanitizeDigest32 :: BS.ByteString -> BS.ByteString
sanitizeDigest32 = BS.map safeChar
    where
    safeChar = \c -> case c of
        'A' -> '0'
        'B' -> '1'
        'C' -> '2'
        'D' -> '3'
        'E' -> '4'
        'F' -> '5'
        'G' -> '6'
        'H' -> '7'
        'I' -> '8'
        'J' -> '9'
        'K' -> 'a'
        'L' -> 'b'
        'M' -> 'c'
        'N' -> 'd'
        'O' -> 'f'
        'P' -> 'g'
        'Q' -> 'h'
        'R' -> 'i'
        'S' -> 'j'
        'T' -> 'k'
        'U' -> 'l'
        'V' -> 'm'
        'W' -> 'n'
        'X' -> 'p'
        'Y' -> 'q'
        'Z' -> 'r'
        '2' -> 's'
        '3' -> 'v'
        '4' -> 'w'
        '5' -> 'x'
        '6' -> 'y'
        '7' -> 'z'
        _   -> error $ "Unexpected character in hash: " ++ show c
