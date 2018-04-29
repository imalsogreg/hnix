{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE PackageImports  #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE LambdaCase #-}

module Nix.Hash where

import qualified Data.ByteString as BS
import Data.Bits
import GHC.Word
import Data.Memory.Encoding.Base16
import Data.Memory.Encoding.Base32
import System.IO.Unsafe
import Prelude hiding (length)
import Foreign.Ptr
-- import qualified "memory" Data.ByteArray.Types as Mem
import Foreign.ForeignPtr
import Data.Memory.PtrMethods
import Data.ByteArray (unsafeCreate)
-- import Data.Memory
import qualified Data.ByteString as Bytestring
import qualified Data.ByteString.Internal as Bytestring

-- import           Data.ByteArray.Types
-- import qualified Data.ByteArray.Types        as B
-- import qualified Data.ByteArray.Methods      as B
-- import           Data.Memory.Internal.Compat

-- -- | Hash algorithms present to nix
-- data HashType = MD5 | SHA1 | SHA256 | SHA512
--   deriving (Eq, Enum, Show)

-- | Class to Access size properties and data of a ByteArray
class ByteArrayAccess ba where
    -- | Return the length in bytes of a bytearray
    length        :: ba -> Int
    -- | Allow to use using a pointer
    withByteArray :: ba -> (Ptr p -> IO a) -> IO a
    -- | Copy the data of a bytearray to a ptr
    copyByteArrayToPtr :: ba -> Ptr p -> IO ()
    copyByteArrayToPtr a dst = withByteArray a $ \src -> memCopy (castPtr dst) src (length a)

-- | Class to allocate new ByteArray of specific size
class (Eq ba, Ord ba, Monoid ba, ByteArrayAccess ba) => ByteArray ba where
    -- | allocate `n` bytes and perform the given operation
    allocRet  :: Int
                -- ^ number of bytes to allocate. i.e. might not match the
                -- size of the given type `ba`.
              -> (Ptr p -> IO a)
              -> IO (a, ba)

instance ByteArrayAccess Bytestring.ByteString where
    length = Bytestring.length
    withByteArray (Bytestring.PS fptr off _) f = withForeignPtr fptr $ \ptr -> f $! (ptr `plusPtr` off)

instance ByteArray Bytestring.ByteString where
    allocRet sz f = do
        fptr <- Bytestring.mallocByteString sz
        r    <- withForeignPtr fptr (f . castPtr)
        return (r, Bytestring.PS fptr 0 sz)

-- -- | Size (in bytes) of various hashes
-- hashTypeSize :: HashType -> Int
-- hashTypeSize = \case
--     MD5    -> 16
--     SHA1   -> 20
--     SHA256 -> 32
--     SHA512 -> 64

-- newtype Hash (t :: HashType) = Hash { hashBytes :: BS.ByteString }

-- class HashSize a where
--     hashSize :: a -> Int

-- instance HashSize (Hash 'MD5) where
--     hashSize _ = hashTypeSize MD5

-- instance HashSize (Hash 'SHA1) where
--     hashSize _ = hashTypeSize SHA1

-- instance HashSize (Hash 'SHA256) where
--     hashSize _ = hashTypeSize SHA256

-- instance HashSize (Hash 'SHA512) where
--     hashSize _ = hashTypeSize SHA512


-- -- | Text encoding shemes for hashes
-- data Base = Base16 | Base32 | Base64
--   deriving (Eq, Ord, Show)

-- readHash32' :: BS.ByteString -> Text

-- printHash32 :: Hash a -> BS.ByteString
-- printHash32 = undefined

-- readHash32 :: BS.ByteString -> Maybe (Hash a)
-- readHash32 = undefined

-- |
-- Module      : Data.ByteArray.Encoding
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- ByteArray base converting
--

-- ***** Copied from `memory`
-- | Convert a bytearray to the equivalent representation in a specific Base
convertToBase32 :: forall bin bout. (ByteArrayAccess bin, ByteArray bout) => bin -> bout
-- convertToBase32 :: BS.ByteString -> BS.ByteString
convertToBase32 b =
    -- Base16 -> doConvert (binLength * 2) toHexadecimal
    let (q,r)  = binLength `divMod` 5
        outLen = 8 * (if r == 0 then q else q + 1)
    in doConvert outLen toBase32
  where
    binLength = length b

    doConvert :: Int -> (Ptr Word8 -> Ptr Word8 -> Int -> IO ()) -> bout
    doConvert l f =
        unsafeCreate l $ \bout ->
        withByteArray b     $ \bin  ->
            f bout bin binLength

-- | Try to Convert a bytearray from the equivalent representation in a specific Base
convertFromBase :: (ByteArrayAccess bin, ByteArray bout) => bin -> Either String bout
convertFromBase b = unsafeDupablePerformIO $
    withByteArray b $ \bin -> do
        mDstLen <- unBase32Length bin (length b)
        case mDstLen of
            Nothing     -> return $ Left "base32: input: invalid length"
            Just dstLen -> do
                (ret, out) <- allocRet dstLen $ \bout -> fromBase32 bout bin (length b)
                case ret of
                    Nothing  -> return $ Right out
                    Just ofs -> return $ Left ("base32: input: invalid encoding at offset: " ++ show ofs)
