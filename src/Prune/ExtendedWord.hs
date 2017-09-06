{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -fno-warn-orphans #-}
module Prune.ExtendedWord
 (
    Word64, Word160,
    word64ToBytes,  bytesToWord64,
    word160ToBytes, bytesToWord160,
 ) where

import           Data.Binary
import           Data.Bits
import           Data.DoubleWord (Word160)

word64ToBytes :: Word64 -> [Word8]
word64ToBytes word = map (fromIntegral . (word `shiftR`)) [64-8, 64-16..0]

bytesToWord64 :: [Word8] -> Word64
bytesToWord64 bytes | length bytes == 8 =
  sum $ map (\(shiftBits, byte) -> fromIntegral byte `shiftL` shiftBits) $ zip [64-8,64-16..0] bytes
bytesToWord64 _ = error "bytesToWord64 was called with the wrong number of bytes"

word160ToBytes :: Word160 -> [Word8]
word160ToBytes word = map (fromIntegral . (word `shiftR`)) [160-8, 160-16..0]

bytesToWord160 :: [Word8] -> Word160
bytesToWord160 bytes | length bytes == 20 =
  sum $ map (\(shiftBits, byte) -> fromIntegral byte `shiftL` shiftBits) $ zip [160-8,160-16..0] bytes
bytesToWord160 _ = error "bytesToWord160 was called with the wrong number of bytes"



