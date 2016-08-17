{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Main where

import Prelude hiding ((.), id)
import Control.Category
import           Criterion.Main
import qualified Data.ByteString.Builder as L
import qualified Data.ByteString.Lazy as L
import           Data.Foldable
import           Data.Monoid
import           Data.Proxy
import           Data.Type.BitRecords
import           Data.Type.BitRecords.Builder.PolyBuilder
import           Data.Type.BitRecords.Builder.StaticPolyBuilder
import           Data.Type.Equality
import           Data.Word
import           GHC.TypeLits ()
import           Test.TypeSpecCrazy

#ifdef FULLBENCHMARKS

type Static64 =
      Field 3 := 2
  :>: Field 5 := 4
  :>: Field 9 := 333
  :>: Field 7 := 35
  :>: Field 30 := 458329
  :>: Field 2 := 1
  :>: Field 2 := 0
  :>: Field 2 := 1
  :>: Field 4 := 9

type Static128 = Field 128 := 0xdeadbeef

type Static256 =
  Static64 :>: Static128 :>: Static64

type Static517 =
   Static256 :>: Static256 :>: Field 5 := 0

#else

type Static64 = Field 64

#endif

aboutStatic64 ::

  "Static Test Type Sizes"
  ########################

     It's "64 bit long" (ShouldBe 64 (GetRecordSize Static64))
#ifdef FULLBENCHMARKS
     -* It's "128 bit long" (ShouldBeTrue ((GetRecordSize Static128) == 128))
     -* It's "256 bit long" (ShouldBeTrue ((GetRecordSize Static256) == 256))
     -* It's "517 bit long" (ShouldBeTrue ((GetRecordSize Static517) == 517))
#endif

aboutStatic64 =
  Valid

lumpUp :: Int -> L.Builder -> [Word8]
lumpUp m = L.unpack . L.toLazyByteString . mconcat . replicate m

static64 m =
  lumpUp m $ toBuilder $ formatBits (Proxy :: Proxy Static64)

#ifdef FULLBENCHMARKS

static128 m =
  lumpUp m $ toBuilder $ formatBits (Proxy :: Proxy Static128)

static256 m =
  lumpUp m $ toBuilder $ formatBits (Proxy :: Proxy Static256)

static517 m =
  lumpUp m $ toBuilder $ formatBits (Proxy :: Proxy Static517)

staticPlain512bitBaseline m =
  lumpUp m $ toBuilder $ formatBits
    (Proxy :: Proxy (
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64 :>:
      Field 64 :>: Field 64 :>: Field 64 :>: Field 64
    ))
#endif

main = do
  print aboutStatic64
  defaultMain
    [bgroup "static-records"
      [

#ifdef FULLBENCHMARKS
      bgroup "64-bit record"
              [bench "1" $ nf static64 1
              ,bench "5" $  nf static64 5
              ,bench "100" $  nf static64 100
              ]
      ,bgroup "128-bit record"
              [bench "1" $ nf static128 1
              ,bench "5" $  nf static128 5
              ,bench "100" $  nf static128 100
              ]
      ,bgroup "256-bit record"
              [bench "1" $ nf static256 1
              ,bench "5" $  nf static256 5
              ,bench "100" $  nf static256 100
              ]
      ,bgroup "517-bit record"
              [bench "1" $ nf static517 1
              ,bench "5" $  nf static517 5
              ,bench "100" $  nf static517 100
              ]
      ,bgroup "512-bit record baseline"
              [bench "1" $ nf staticPlain512bitBaseline 1
              ,bench "5" $  nf staticPlain512bitBaseline 5
              ,bench "100" $  nf staticPlain512bitBaseline 100
              ]
#else
      bgroup "static 64-bit"
              [bench "1" $ nf static64 1
              ,bench "5" $  nf static64 5
              ,bench "100" $  nf static64 100
              ]
#endif
      ,bgroup "Builder"
        [bgroup "Static"
          [bgroup "Poly" []
          ,bgroup "ByteStringBuilder" []
          ]
        ,bgroup "Runtime"
          [bgroup "Poly" [bgroup "BittrBufferUnlimited-direct"
                                [bench "1" $ nf bittrBufferUnlimitedDirect 1
                                ,bench "5" $ nf bittrBufferUnlimitedDirect 5
                                ,bench "100" $ nf bittrBufferUnlimitedDirect 100
                                ]
                          ,bgroup "BittrBufferUnlimited-typeclass"
                                [bench "1" $ nf bittrBufferUnlimitedTypeClass 1
                                ,bench "5" $ nf bittrBufferUnlimitedTypeClass 5
                                ,bench "100" $ nf bittrBufferUnlimitedTypeClass 100
                                ]
                          ,bgroup "BittrBufferUnlimited-typeclass-M-Times"
                                [bench "1" $ nf bittrBufferUnlimitedTypeClassMTimes 1
                                ,bench "5" $ nf bittrBufferUnlimitedTypeClassMTimes 5
                                ,bench "100" $ nf bittrBufferUnlimitedTypeClassMTimes 100
                                ]
                          ,bgroup "BittrBufferWord64-direct"
                                [bench "1" $ nf bittrBufferWord64Direct 1
                                ,bench "5" $ nf bittrBufferWord64Direct 5
                                ,bench "100" $ nf bittrBufferWord64Direct 100
                                ]
                          ]
           ]
          ]
        ]
      ]

bittrBufferUnlimitedDirect m =
  lumpUp 1
    $ runBittrWriter
    $ appendUnlimited
    $ BittrBufferUnlimited 0x01020304050607 (64 * m)

bittrBufferUnlimitedTypeClass m =
  lumpUp 1
    $ runBittrWriterHoley
    $ toHoley
    $ BittrBufferUnlimited 0x01020304050607 (64 * m)

bittrBufferUnlimitedTypeClassMTimes m =
  lumpUp 1
    $ runBittrWriterHoley
    $ foldr (.) id
    $ replicate m
    $ toHoley (BittrBufferUnlimited 0x01020304050607 64)

bittrBufferWord64Direct m =
  lumpUp 1
    $ runBittrWriter
    $ mconcat
    $ replicate m
    $ appendBittrBuffer
    $ BittrBuffer 0x01020304050607 64
