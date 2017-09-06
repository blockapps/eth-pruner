{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Prune.Types where

import           Data.Binary
import qualified Data.ByteString                              as B
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics

import           Blockchain.Data.RLP
import           Blockchain.Database.MerklePatricia.StateRoot (StateRoot (..))
import           Prune.ExtendedWord


type SHA = B.ByteString

newtype Address = Address Word160 deriving (Show, Eq, Read, Enum, Real, Bounded, Num, Ord, Generic, Integral)

instance RLPSerializable Address where
  rlpEncode (Address a) = RLPString $ B.pack $ word160ToBytes a
  rlpDecode (RLPString s) = Address $ bytesToWord160 $ B.unpack s
  rlpDecode x             = error ("Malformed rlp object sent to rlp2Address: " ++ show x)

data BlockHeader =
  BlockHeader {
    parentHash       :: SHA,
    ommersHash       :: SHA,
    beneficiary      :: Address,
    stateRoot        :: StateRoot,
    transactionsRoot :: StateRoot,
    receiptsRoot     :: StateRoot,
    logsBloom        :: B.ByteString,
    difficulty       :: Integer,
    number           :: Integer,
    gasLimit         :: Integer,
    gasUsed          :: Integer,
    timestamp        :: UTCTime,
    extraData        :: Integer,
    mixHash          :: SHA,
    nonce            :: Word64
    } deriving (Eq, Read, Show)

instance RLPSerializable BlockHeader where
  rlpEncode (BlockHeader ph oh b sr tr rr lb d number' gl gu ts ed mh nonce') =
    RLPArray [
      rlpEncode ph,
      rlpEncode oh,
      rlpEncode b,
      rlpEncode sr,
      rlpEncode tr,
      rlpEncode rr,
      rlpEncode lb,
      rlpEncode d,
      rlpEncode number',
      rlpEncode gl,
      rlpEncode gu,
      rlpEncode (round $ utcTimeToPOSIXSeconds ts::Integer),
      rlpEncode ed,
      rlpEncode mh,
      rlpEncode $ B.pack $ word64ToBytes nonce'
      ]
  rlpDecode (RLPArray [ph, oh, b, sr, tr, rr, lb, d, number', gl, gu, ts, ed, mh, nonce']) =
    BlockHeader {
      parentHash=rlpDecode ph,
      ommersHash=rlpDecode oh,
      beneficiary=rlpDecode b,
      stateRoot=rlpDecode sr,
      transactionsRoot=rlpDecode tr,
      receiptsRoot=rlpDecode rr,
      logsBloom=rlpDecode lb,
      difficulty=rlpDecode d,
      number=rlpDecode number',
      gasLimit=rlpDecode gl,
      gasUsed=rlpDecode gu,
      timestamp=posixSecondsToUTCTime $ fromInteger $ rlpDecode ts,
      extraData=rlpDecode ed,
      mixHash=rlpDecode mh,
      nonce=bytesToWord64 $ B.unpack $ rlpDecode nonce'
      }
  rlpDecode x = error $ "can not run rlpDecode on BlockHeader for value " ++ show x

data AddressState =
  AddressState { addressStateNonce        :: Integer
               , addressStateBalance      :: Integer
               , addressStateContractRoot :: StateRoot
               , addressStateCodeHash     :: SHA
               }
    deriving (Eq, Read, Show)

instance RLPSerializable AddressState where
  rlpEncode a | addressStateBalance a < 0 = error $ "Error in cal to rlpEncode for AddressState: AddressState has negative balance: " ++ show a
  rlpEncode a = RLPArray [
    rlpEncode $ toInteger $ addressStateNonce a,
    rlpEncode $ toInteger $ addressStateBalance a,
    rlpEncode $ addressStateContractRoot a,
    rlpEncode $ addressStateCodeHash a
                ]

  rlpDecode (RLPArray [n, b, cr, ch]) =
    AddressState {
      addressStateNonce=fromInteger $ rlpDecode n,
      addressStateBalance=fromInteger $ rlpDecode b,
      addressStateContractRoot=rlpDecode cr,
      addressStateCodeHash=rlpDecode ch
      }
  rlpDecode x = error $ "Missing case in rlpDecode for AddressState: " ++ show x
