{-# LANGUAGE UndecidableInstances #-}
module BitRecordsSpec (spec) where

import Data.Bits
import Data.Type.BitRecords
import Data.Type.BitRecords.DynByteStringBuilder
import Data.Proxy
import Data.Word
import Data.Type.Equality ()
import Data.ByteString.Builder
import GHC.TypeLits
import Test.Hspec
import Test.TypeSpecCrazy
import Text.Printf
import qualified Data.ByteString.Lazy as B
import Prelude hiding ((.), id)
import Data.Tagged
import Test.QuickCheck (property)

type TestRecAligned =
  "bar" :=> Field 8       :>:
            Field 8  := 0 :>:
  "baz" :=> Field 8       :>:
            Field 32 := 0 :>:
  "foo" :=> Field 8       :>:
            Field 8  := 0 :>:
  "oof" :=> Field 8       :>:
            Field 8  := 0 :>:
  "rab" :=> Field 8

checkTestRecAligned
  :: Expect '[ ShouldBe 96        (GetRecordSize TestRecAligned)
             , ShouldBe '(0, 7)   (GetFieldPositionUnsafe TestRecAligned "bar")
             , ShouldBe '(16, 23) (GetFieldPositionUnsafe TestRecAligned "baz")
             , ShouldBe '(56, 63) (GetFieldPositionUnsafe TestRecAligned "foo")
             , ShouldBe '(72, 79) (GetFieldPositionUnsafe TestRecAligned "oof")
             , ShouldBe '(88, 95) (GetFieldPositionUnsafe TestRecAligned "rab")
             ]
checkTestRecAligned = Valid

type TestRecUnAligned =
  "bar" :=> Field 8       :>:
            Field 8  := 0 :>:
  "baz" :=> Field 7       :>:
            Field 32 := 0 :>:
  "foo" :=> Field 8       :>:
            Field 8  := 0xfe

checkTestRecUnAligned
  :: Expect '[ ShouldBe 71        (GetRecordSize TestRecUnAligned)
             , ShouldBe '(0, 7)   (GetFieldPositionUnsafe TestRecUnAligned "bar")
             , ShouldBe '(16, 22) (GetFieldPositionUnsafe TestRecUnAligned "baz")
             , ShouldBe '(55, 62) (GetFieldPositionUnsafe TestRecUnAligned "foo")
             ]
checkTestRecUnAligned = Valid

type TestHasField =
       "foo" :=> Flag
   :>:           Field 4
   :>: "bar" :=> Field 2
testBitHasFields
  :: Expect '(ShouldBeTrue (HasField TestHasField "foo"),
              ShouldBeTrue (HasField TestHasField "bar"))
testBitHasFields = Valid

testTakeLastN ::
  "Taking the last n elements of a list"
  #######################################

       TakeLastN 0 '[1,2,3] `ShouldBe` ('[] :: [Nat])
    -* TakeLastN 1 '[1,2,3] `ShouldBe` '[3]
    -* TakeLastN 2 '[1,2,3] `ShouldBe` '[2,3]
    -* TakeLastN 5 '[1,2,3] `ShouldBe` '[1,2,3]

testTakeLastN = Valid


testRem
  :: Expect '[ Rem 0 3 `ShouldBe` 0
             , Rem 1 3 `ShouldBe` 1
             , Rem 2 3 `ShouldBe` 2
             , Rem 3 3 `ShouldBe` 0
             , Rem 4 3 `ShouldBe` 1
             , Rem 5 3 `ShouldBe` 2
             , Rem 6 3 `ShouldBe` 0
            ]
testRem = Valid

testRemPow2
  ::
  "RemPow2"
  #########

  "Remainder of '1'"
  ~~~~~~~~~~~~~~~~~~

      It "1 `RemPow2` 1 is 1"  (Is 1 (RemPow2 1 1))
  -*  It "1 `RemPow2` 8 is 1"  (Is 1 (RemPow2 1 8))

  -/-

   "Remainder of '3916441'"
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~

      It " `RemPow2` 1 is 1"   (Is 1 (RemPow2 3916441 1))
  -*  It " `RemPow2` 4 is 9"   (Is 9 (RemPow2 3916441 4))
  -*  It " `RemPow2` 8 is 153" (Is 153 (RemPow2 3916441 8))

testRemPow2 = Valid

testDiv
  :: Expect '[ Div 0 3 `ShouldBe` 0
             , Div 1 3 `ShouldBe` 0
             , Div 2 3 `ShouldBe` 0
             , Div 3 3 `ShouldBe` 1
             , Div 4 3 `ShouldBe` 1
             , Div 5 3 `ShouldBe` 1
             , Div 6 3 `ShouldBe` 2
             , Div 144 13 `ShouldBe` 11
             -- , Div 512 128 `ShouldBe` 11
            ]
testDiv = Valid

testNatBits
  :: "Type Level bit operations"
     ###########################

     "TestHighBit"
     ~~~~~~~~~~~~

        ShouldBeFalse (TestHighBit 127 8)
     -* ShouldBeFalse (TestHighBit 127 7)
     -* ShouldBeTrue  (TestHighBit 127 6)
     -* ShouldBeFalse (TestHighBit  32 6)
     -* ShouldBeTrue  (TestHighBit  32 5)
     -* ShouldBeFalse (TestHighBit  16 5)
     -* ShouldBeTrue  (TestHighBit  16 4)
     -* ShouldBeFalse (TestHighBit   8 4)
     -* ShouldBeTrue  (TestHighBit   8 3)
     -* ShouldBeFalse (TestHighBit   4 3)
     -* ShouldBeTrue  (TestHighBit   4 2)
     -* ShouldBeFalse (TestHighBit   2 2)
     -* ShouldBeTrue  (TestHighBit   2 1)
     -* ShouldBeFalse (TestHighBit   0 1)
     -* ShouldBeTrue  (TestHighBit   1 0)

  -/-

    "ToBits"
    ~~~~~~~~~

       It "returns the empty list for a zero bit length"
          (ToBits 1023 0 `ShouldBe` ('[] :: [Bool]) )

    -* It "returns [] for a single unset bit"
          (ShouldBe ('[] :: [Bool]) (ToBits 0 1))

    -* It "returns [True] for a single set bit"
          (ShouldBe '[ 'True] (ToBits 1 1))

    -* It "returns [True, False] when getting two bits from 0x2"
          (ShouldBe '[ 'True, 'False] (ToBits 0x2 2))

    -* It "returns the list of bits in correct order"
          (ShouldBe '[ 'True, 'True, 'True, 'True
                     , 'False, 'False, 'False, 'False]
                     (ToBits 0xf0 8))

    -* It "returns no leading 'False (i.e. it omits leading zero bits)"
          (ShouldBe '[ 'True, 'True, 'True, 'True]
                     (ToBits 0x0000000f 32))
  -/-

    "FromBits"
    ~~~~~~~~~

      It "returns 0 for '[]"
          (ShouldBe 0   (FromBits ('[] :: [Bool])))

    -* It "returns 0 for [False]"
          (ShouldBe 0   (FromBits '[ 'False]))

    -* It "returns 1 for [True]"
          (ShouldBe 1   (FromBits '[ 'True]))

    -* It "returns 2 for [True, False]"
          (ShouldBe 2   (FromBits '[ 'True, 'False]))

    -* It "returns 4 for [True, False, False]"
          (ShouldBe 4   (FromBits '[ 'True, 'False, 'False]))

    -* It "returns 5 for [True, False, True]"
          (ShouldBe 5   (FromBits '[ 'True, 'False, 'True]))
  -/-

    "ShiftBitsR"
    ~~~~~~~~~~~~

      It "returns the input bits for n == 0"
        (ShouldBe '[ 'True, 'False] (ShiftBitsR ['True, 'False] 0))

    -* It "returns '[] when shifting [True] 1 bits"
        (ShouldBe ('[] :: [Bool]) (ShiftBitsR '[ 'True ] 1))

    -* It "returns '[True] when shifting [True, True] 1 bits"
        (ShouldBe '[ 'True] (ShiftBitsR '[ 'True, 'True ] 1))

    -* It "returns (ToBits 12 8) when shifting (ToBits 97 8) 3 bits to the right"
        (ShouldBe (ToBits 12 8) (ShiftBitsR (ToBits 97 8) 3))
  -/-

     "GetMostSignificantBitIndex"
    ~~~~~~~~~~~~~~~

      It "returns 1 for 0"
        (ShouldBe 1 (GetMostSignificantBitIndex 64 0))

    -* It "returns 1 for 1"
        (ShouldBe 1 (GetMostSignificantBitIndex 64 1))

    -* It "returns 1 for 2"
        (ShouldBe 1 (GetMostSignificantBitIndex 64 2))

    -* It "returns 1 for 3"
        (ShouldBe 1 (GetMostSignificantBitIndex 64 3))

    -* It "returns 2 for 4"
        (ShouldBe 2 (GetMostSignificantBitIndex 64 4))

    -* It "returns 2 for 5"
        (ShouldBe 2 (GetMostSignificantBitIndex 64 4))

    -* It "returns 8 for 511"
        (ShouldBe 8 (GetMostSignificantBitIndex 64 511))

    -* It "returns 63 for (2^64 - 1)"
        (ShouldBe 63 (GetMostSignificantBitIndex 64 (2^64 - 1)))

  -/-


    "ShiftR"
    ~~~~~~~~~

       It "returns '0' when shifting '42' 6 bits to the right"
        (ShouldBe 0 (ShiftR 64 42 6))
    -* It "returns 2 when shifting 512 8 bits to the right"
        (ShouldBe 2 (ShiftR 64 512 8))

testNatBits = Valid

testAlign
  :: Expect '[ Align 'True 7 Flag       `ShouldBe`  (Flag :>: Field 6 := 0)
             , Align 'True 1 Flag       `ShouldBe`  Flag
             , Align 'True 8 (Field 7)  `ShouldBe`  (Field 7 :>: Field 1 := 0)
             , Align 'True 8 (Field 8)  `ShouldBe`  Field 8
             , Align 'True 8 (Field 9)  `ShouldBe`  (Field 9 :>: Field 7 := 0)
            ]
testAlign = Valid

type TestField0 =  "test" :=> Field 19
testFieldPosition0
   :: Expect (GetFieldPositionUnsafe TestField0 "test" `ShouldBe` '(0,18))
testFieldPosition0 = Valid

type TestField1 =
      Field 1
  :>: "foo" :=> Flag
  :>: Field 8
  :>: "bar" :=> Field 5
  :>: "baz" :=> Field 9

testFieldPosition1Foo
   :: Expect (GetFieldPositionUnsafe TestField1 "foo" `ShouldBe` '(1,1))
testFieldPosition1Foo = Valid

testFieldPosition1Bar
   :: Expect (GetFieldPositionUnsafe TestField1 "bar" `ShouldBe` '(10,14))
testFieldPosition1Bar = Valid

testFieldPosition1Baz
   :: Expect (Try (GetFieldPosition TestField1 "baz") `ShouldBe` '(15,23))
testFieldPosition1Baz = Valid

testFieldPositionToList
   :: Expect
       (FieldPostitionToList
         '(15,23)
       `ShouldBe`
       '[15,16,17,18,19,20,21,22,23])
testFieldPositionToList = Valid

testGetRemainingUnaligned ::
  TypeSpec '[ GetRemainingUnaligned 1 'Align32  `ShouldBe` 1
            , GetRemainingUnaligned 16 'Align16  `ShouldBe` 0
            , GetRemainingUnaligned 31 'Align16  `ShouldBe` 15
            ]
testGetRemainingUnaligned = Valid

spec :: Spec
spec = do
  describe "The Set of Type Functions" $
    it "is sound" $ do
      print (Valid :: Expect (GetRecordSize (Flag :>: Field 7) `Is` 8))
      print testBitHasFields
      print testTakeLastN
      print testRem
      print testRemPow2
      print testDiv
      print testNatBits
      print testAlign
      print testFieldPosition0
      print testFieldPosition1Foo
      print testFieldPosition1Bar
      print testFieldPosition1Baz
      print testFieldPositionToList
      print checkTestRecAligned
      print checkTestRecUnAligned
      print testGetRemainingUnaligned
  describe "showRecord" $ do
    it "prints (Field 4 :>: (Field 4 := 0x96)) to \"<..>0110\"" $
      let actual = showRecord (Proxy :: Proxy (Field 4 :>: (Field 4 := 0x96)))
          in actual `shouldBe` "<..>0110"
  describe "Field-Getter" $ do
    it "returns False for flag 'foo'" $
      getFlag
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy ("no" :=> Flag :>: "foo" :=> Flag ))
        (0xd :: Word8)
       `shouldBe` False
    it "returns True for flag 'foo'" $
      getFlag
         (Proxy :: Proxy "foo")
         (Proxy :: Proxy ("no" :=> Flag :>: "foo" :=> Flag ))
         (0xf :: Word8)
       `shouldBe` True
    it "returns 0x1 for field 'foo'" $
      getField
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy (Field 16 :>: "foo" :=> Flag))
        (0x00010000 :: Word32)
       `shouldBe` (1 :: Word32)
    it "returns 0xcafe for field 'foo'" $
      getField
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy (Field 13 :>: "foo" :=> Field 16))
        (0xcafe0000 `shiftR` 3 :: Word32)
       `shouldBe` (0xcafe :: Word32)
    it "returns 0xcafe for field 'foo'" $
      getField
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy (Field 13
                         :>: "foo" :=> (    Field 7
                                        :>: Flag
                                        :>: Field 6
                                        :>: Flag
                                        :>: Flag)))
        (0xcafe0000 `shiftR` 3 :: Word32)
       `shouldBe` (0xcafe :: Word32)
  describe "formatAlignedBits" $ do
        let rec = Proxy
            rec :: Proxy TestRecAligned
            actual = B.unpack $ toLazyByteString actualB
              where actualB = toBuilder $ formatAlignedBits rec
                        (Tagged 1 :: Tagged "bar" Integer)
                        (Tagged 2 :: Tagged "baz" Integer)
                        (Tagged 4 :: Tagged "foo" Integer)
                        (Tagged 8 :: Tagged "oof" Integer)
                        (Tagged 16 :: Tagged "rab" Integer)
        it "writes fields in big endian" $
            actual `shouldBe` [1,0,2,0,0,0,0,4,0,8,0,16]
  describe "unaligned bit records" $ do
    describe "formatBits align64 big endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          align64
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in  actual `shouldBe`
                  "<< 01 00 06 00 00 00 00 0f fc 00 00 00 00 00 00 00 >>"
    describe "formatBits align32 big endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          align32
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in  actual `shouldBe` "<< 01 00 06 00 00 00 00 0f fc 00 00 00 >>"
    describe "formatBits align16 big endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          align16
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in  actual `shouldBe` "<< 01 00 06 00 00 00 00 0f fc 00 >>"
    describe "formatBits align8" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $ formatBits align8 rec
                        (Tagged 1 :: Tagged "bar" Integer)
                        (Tagged 3 :: Tagged "baz" Integer)
                        (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in actual `shouldBe` "<< 01 00 06 00 00 00 00 0f fc >>"
    describe "Formatting sub-byte fields" $ do
      it "only the addressed bits are copied to the output" $
        property $ \value ->
          let rec = Proxy
              rec :: Proxy (Field 4 := 0 :>: "here" :=> Field 4)
              actualB :: Builder
              actualB = toFlushedBuilder $ formatBits align8 rec
                          (Tagged value :: Tagged "here" Integer)
              actual = printBuilder actualB
              expected = printf "<< %.2x >>" (value .&. 0xf)
              in actual `shouldBe` expected
      it "renders (Flag := 0 :>: (Field 7 := 130)) to << 02 >>" $
        let rec = Proxy
            rec :: Proxy (Flag := 0 :>: (Field 7 := 130))
            actual = printBuilder b
              where b = toBuilder $ formatAlignedBits rec
        in actual `shouldBe` "<< 02 >>"
    describe "DynByteStringBuilder" $
      describe "appendUnlimited" $
        it "0x01020304050607 to << 00 01 02 03 04 05 06 07 >>" $
          let expected = "<< 00 01 02 03 04 05 06 07 >>"
              actual =
                printBuilder
                  (getAndRunBittrWriterHoley
                      (BittrBufferUnlimited 0x01020304050607 64))
              in actual `shouldBe` expected
-- * Bit Buffering
