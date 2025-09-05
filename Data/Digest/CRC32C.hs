{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Digest.CRC32C
  ( CRC32C (..)
  ) where

import qualified Data.ByteString                as BS
import qualified Data.ByteString.Lazy           as BL
import qualified Data.ByteString.Short          as BSS
import           Data.Word

#if !MIN_VERSION_bytestring(0, 11, 1)
import qualified Data.ByteString.Short.Internal as BSS
#endif

import qualified "crc32c" Data.Digest.CRC32C as CRC32C

class CRC32C a where
  -- | Compute CRC32C checksum
  crc32c :: a -> Word32
  crc32c = crc32cUpdate 0

  -- | Given the CRC32C checksum of a string, compute CRC32C of its
  -- concatenation with another string (t.i., incrementally update
  -- the CRC32C hash value)
  crc32cUpdate :: Word32 -> a -> Word32

instance CRC32C BS.ByteString where
  crc32c = CRC32C.crc32c

  crc32cUpdate = CRC32C.crc32c_update

instance CRC32C BL.ByteString where
  crc32cUpdate = BL.foldlChunks crc32cUpdate

instance CRC32C [Word8] where
  crc32cUpdate n = (crc32cUpdate n) . BL.pack

instance CRC32C BSS.ShortByteString where
  crc32c = crc32c . BSS.fromShort

  crc32cUpdate cks = crc32cUpdate cks . BSS.fromShort
