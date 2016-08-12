{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Main where

import Criterion.Main
import Data.Type.BitRecords
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as L
import Data.Word
import Data.Type.Equality
import Data.Proxy
import Test.TypeSpecCrazy
import GHC.TypeLits ()

type Static64 =
  Field 3 := 2 :>: Field 5 := 4 :>: Field 9 := 333 :>: Field 7 := 35 :>: Field 30 := 458329 :>: Field 2 := 1 :>: Field 2 := 0 :>: Field 2 := 1 :>: Field 4 := 9

#ifdef FULLBENCHMARKS
type Static128 = Field 128 := 0xdeadbeef

type Static256 =
  Static64 :>: Static128 :>: Static64

type Static517 =
   Static256 :>: Static256 :>: Field 5 := 0
#endif

aboutStatic64 ::

  "Static Test Type Sizes"
  ########################

     It's "64 bit long" (ShouldBeTrue ((GetRecordSize Static64) == 64))
#ifdef FULLBENCHMARKS
     -* It's "128 bit long" (ShouldBeTrue ((GetRecordSize Static128) == 128))
     -* It's "256 bit long" (ShouldBeTrue ((GetRecordSize Static256) == 256))
     -* It's "517 bit long" (ShouldBeTrue ((GetRecordSize Static517) == 517))
#endif

aboutStatic64 =
  Valid

lumpUp :: Int -> L.Builder -> [Word8]
lumpUp m = L.unpack . L.toLazyByteString . mconcat . replicate m

staticAutoAligned64 m =
  lumpUp m $ toBuilder $ formatAlignedBits (Proxy :: Proxy Static64)

static8BitAligned64 m =
  lumpUp m $ toBuilder $ formatBits align8 (Proxy :: Proxy Static64)
static16BitAligned64 m =
  lumpUp m $ toBuilder $ formatBits align16 (Proxy :: Proxy Static64)
static32BitAligned64 m =
  lumpUp m $ toBuilder $ formatBits align32 (Proxy :: Proxy Static64)
static64BitAligned64 m =
  lumpUp m $ toBuilder $ formatBits align64 (Proxy :: Proxy Static64)

#ifdef FULLBENCHMARKS

staticAutoAligned128 m =
  lumpUp m $ toBuilder $ formatAlignedBits (Proxy :: Proxy Static128)
static8BitAligned128 m =
  lumpUp m $ toBuilder $ formatBits align8 (Proxy :: Proxy Static128)
static32BitAligned128 m =
  lumpUp m $ toBuilder $ formatBits align32 (Proxy :: Proxy Static128)
static64BitAligned128 m =
  lumpUp m $ toBuilder $ formatBits align64 (Proxy :: Proxy Static128)

staticAutoAligned256 m =
  lumpUp m $ toBuilder $ formatAlignedBits (Proxy :: Proxy Static256)
static8BitAligned256 m =
  lumpUp m $ toBuilder $ formatBits align8 (Proxy :: Proxy Static256)
static32BitAligned256 m =
  lumpUp m $ toBuilder $ formatBits align32 (Proxy :: Proxy Static256)
static64BitAligned256 m =
  lumpUp m $ toBuilder $ formatBits align64 (Proxy :: Proxy Static256)

static8BitAligned517 m =
  lumpUp m $ toFlushedBuilder $ formatBits align8 (Proxy :: Proxy Static517)
static64BitAligned517 m =
  lumpUp m $ toFlushedBuilder $ formatBits align64 (Proxy :: Proxy Static517)

staticPlain512bitBaseline m =
  lumpUp m $ toBuilder $ formatBits align64
    (Proxy :: Proxy (
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64 :>:
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64
    ))
#endif

main = do
  putStrLn $ show aboutStatic64
  defaultMain
    [bgroup "static-records"
      [

#ifdef FULLBENCHMARKS
      bgroup "auto-align"
              [bench "64bit record" $ nf staticAutoAligned64 1
              ,bench "128bit record" $  nf staticAutoAligned128 1
              ,bench "256bit record" $  nf staticAutoAligned256 1
              ]
      ,bgroup "8 bit aligned"
              [bench "64bit record" $ nf static8BitAligned64 1
              ,bench "128bit record" $  nf static8BitAligned128 1
              ,bench "256bit record" $  nf static8BitAligned256 1
              ,bench "517bit record" $  nf static8BitAligned517 1
              ]
      ,bgroup "16 bit aligned"
              [bench "64bit record" $ nf static16BitAligned64 1
              ]
      ,bgroup "32 bit aligned"
              [bench "64bit record" $ nf static32BitAligned64 1
              ,bench "128bit record" $  nf static32BitAligned128 1
              ,bench "256bit record" $  nf static32BitAligned256 1
              ]
      ,bgroup "64 bit aligned"
              [bench "64bit record" $ nf static64BitAligned64 1
              ,bench "128bit record" $  nf static64BitAligned128 1
              ,bench "256bit record" $  nf static64BitAligned256 1
              ,bench "517bit record" $  nf static64BitAligned517 1
              ]
#else
      bgroup "static 64-bit"
              [bench "64bit aligned" $ nf static64BitAligned64 1
              ,bench "32bit aligned" $ nf static32BitAligned64 1
              ,bench "16bit aligned" $ nf static16BitAligned64 1
              ,bench "8bit aligned" $ nf static8BitAligned64 1
              ,bench "auto aligned" $ nf staticAutoAligned64 1
              ]
#endif
      ]
    ]
