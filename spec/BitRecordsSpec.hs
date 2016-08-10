{-# LANGUAGE UndecidableInstances #-}
module BitRecordsSpec (spec) where

import Data.Bits
import Data.ByteString.IsoBaseFileFormat.Util.BitRecords
import Data.ByteString.IsoBaseFileFormat.Util.PrettyType
import Data.Proxy
import Data.Word
import Data.Type.Equality
import Data.Type.Bool
import Data.Monoid
import Data.ByteString.Builder
import Control.Category
import GHC.TypeLits
import Test.Hspec
import Test.TypeSpecCrazy
import Data.Int
import Text.Printf
import Debug.Trace
import qualified Data.ByteString.Lazy as B
import Data.Kind hiding (type (*))
import Prelude hiding ((.), id)
import Data.Tagged

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


type family PrettyRecord rec :: PrettyType where
  PrettyRecord (Field 0) = 'PrettyEmpty
  PrettyRecord (Field 1) = PutStr "F"
  PrettyRecord (Field n) =
    PutStr "<" <++> PrettyOften (n - 2) (PutStr ".") <++> PutStr ">"
  PrettyRecord (l :=> r) =
    PutStr "<" <++>
    'PrettySymbol ('PrettyPadded ((GetRecordSize r) - 2)) ('PrettyPrecision ((GetRecordSize r) - 2)) l
    <++> PutStr ">"
  PrettyRecord (r :=  v) =
    'PrettyNat 'PrettyUnpadded ('PrettyPrecision (GetRecordSize r)) 'PrettyBit v
  PrettyRecord (l :>: r) = PrettyRecord l <++> PrettyRecord r

printRec
  :: forall proxy (rec :: Type)
  . PrettyTypeShow (PrettyRecord rec)
  => proxy rec -> String
printRec _ = ptShow (Proxy :: Proxy (PrettyRecord rec))

-- 70 .. 63
--
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

type TestHasNestedField =
       "foo" :=> "bar" :=> Field 2
testBitHasNestedFields
  :: Expect (ShouldBeTrue (HasField TestHasNestedField ("foo" :/ "bar")))
testBitHasNestedFields = Valid

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
  :: Expect '[ Align 'True 7 Flag       `ShouldBe`  (Flag :>: Ignore (Field 6 := 0))
             , Align 'True 1 Flag       `ShouldBe`  Flag
             , Align 'True 8 (Field 7)  `ShouldBe`  (Field 7 :>: Ignore (Field 1 := 0))
             , Align 'True 8 (Field 8)  `ShouldBe`  Field 8
             , Align 'True 8 (Field 9)  `ShouldBe`  (Field 9 :>: Ignore (Field 7 := 0))
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

type TestFieldNested =
     Field 13
     :>: "bar" :=> (              Field 7
                    :>:           Flag
                    :>: "foo" :=> Field 16
                    :>:           Flag
                    :>:           Flag)

testFieldPositionTestFieldNested
   :: Expect
       (Try
         (GetFieldPosition TestFieldNested ("bar" :/ "foo"))
             `ShouldBe`
                        '(13 + 7 + 1, (16 - 1) + (13 + 7 + 1)))
testFieldPositionTestFieldNested = Valid

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
      print testBitHasNestedFields
      print testRem
      print testDiv
      print testNatBits
      print testAlign
      print testFieldPosition0
      print testFieldPosition1Foo
      print testFieldPosition1Bar
      print testFieldPosition1Baz
      print testFieldPositionTestFieldNested
      print testFieldPositionToList
      print checkTestRecAligned
      print checkTestRecUnAligned
      print testGetRemainingUnaligned
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
    it "returns 0xcafe for nested field 'bar :/ foo'" $
      getField
        (Proxy :: Proxy ("bar" :/ "foo"))
        (Proxy :: Proxy (              Field 13
                         :>: "bar" :=> (              Field 7
                                        :>:           Flag
                                        :>: "foo" :=> Field 16
                                        :>:           Flag
                                        :>:           Flag)))
        (0xcafe `shiftL` (13 + 7 + 1) :: Word64)
       `shouldBe` (0xcafe :: Word64)
  describe "Field-Setter" $ do
    it "sets nested flag 'bar :/ foo' to 1" $
      setFlag
        (Proxy :: Proxy ("bar" :/ "foo"))
        (Proxy :: Proxy (              Field 13
                         :>: "bar" :=> (              Field 7
                                        :>:           Flag
                                        :>: "foo" :=> Flag
                                        :>:           Flag
                                        :>:           Flag)))
        True
        (0 :: Word64)
       `shouldBe` (1 `shiftL` (13 + 7 + 1) :: Word64)
    it "sets nested field 'bar :/ foo' to 0xe" $
      setField
        (Proxy :: Proxy ("bar" :/ "foo"))
        (Proxy :: Proxy (              Field 13
                         :>: "bar" :=> (              Field 7
                                        :>:           Flag
                                        :>: "foo" :=> Field 4
                                        :>:           Flag
                                        :>:           Flag)))
        (0xe :: Word8)
        (0xcaf0 `shiftL` (13 + 7 + 1) :: Word64)
       `shouldBe` (0xcafe `shiftL` (13 + 7 + 1) :: Word64)
  describe "formatAlignedBits" $ do
        let rec = Proxy
            rec :: Proxy TestRecAligned
            actual e = B.unpack $ toLazyByteString actualB
              where actualB = toBuilder $ formatAlignedBits e rec
                        (Tagged 1 :: Tagged "bar" Integer)
                        (Tagged 2 :: Tagged "baz" Integer)
                        (Tagged 4 :: Tagged "foo" Integer)
                        (Tagged 8 :: Tagged "oof" Integer)
                        (Tagged 16 :: Tagged "rab" Integer)
        it "writes fields in big endian" $
            actual bigEndian `shouldBe` [1,0,2,0,0,0,0,4,0,8,0,16]
        it "writes fields in little endian" $
            -- since the TestRecAligned has a size of 96 bit, it is
            -- automatically aligned to 32 bits, so little endian
            -- of [A,B,C,D, A,B,C,D, A,B,C,D]
            -- is [D,C,B,A, D,C,B,A, D,C,B,A]
            actual littleEndian `shouldBe` [0,2,0,1, 4,0,0,0, 16,0,8,0]
  describe "unaligned bit records" $ do
    describe "formatBits align64 little endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          littleEndian
                          align64
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)

            actual = printBuilder actualB
            in  actual `shouldBe`
                  "<< 0f 00 00 00 00 06 00 01 00 00 00 00 00 00 00 fc >>"
    describe "formatBits align64 big endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          bigEndian
                          align64
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in  actual `shouldBe`
                  "<< 01 00 06 00 00 00 00 0f fc 00 00 00 00 00 00 00 >>"
    describe "formatBits align32 little endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          littleEndian
                          align32
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)

            actual = printBuilder actualB
            in  actual `shouldBe` "<< 00 06 00 01 0f 00 00 00 00 00 00 fc >>"
    describe "formatBits align32 big endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          bigEndian
                          align32
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in  actual `shouldBe` "<< 01 00 06 00 00 00 00 0f fc 00 00 00 >>"
    describe "formatBits align16 little endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          littleEndian
                          align16
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)

            actual = printBuilder actualB
            in  actual `shouldBe` "<< 00 01 00 06 00 00 0f 00 00 fc >>"
    describe "formatBits align16 big endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $
                        formatBits
                          bigEndian
                          align16
                          rec
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Integer)
                          (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in  actual `shouldBe` "<< 01 00 06 00 00 00 00 0f fc 00 >>"
    describe "formatBits align8 (little / ignored) endian" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $ formatBits littleEndian align8 rec
                        (Tagged 1 :: Tagged "bar" Integer)
                        (Tagged 3 :: Tagged "baz" Integer)
                        (Tagged 7 :: Tagged "foo" Integer)
            actual = printBuilder actualB
            in actual `shouldBe` "<< 01 00 06 00 00 00 00 0f fc >>"

-- * Alignment

-- | Alignments for optimized writing of bits into bytestrings.
data Alignment = Align8 | Align16 | Align32 | Align64

-- | A convenience contraint type alias for 'Alignment's.
type KnownAlignment a =
  ( Num (ToAlignedWord a)
  , Show (ToAlignedWord a)
  , Bits (ToAlignedWord a)
  , FiniteBits (ToAlignedWord a)
  , Ord (ToAlignedWord a)
  , Eq (ToAlignedWord a)
  , PrintfArg (ToAlignedWord a)
  , KnownNat (GetAlignmentBits a)
  , KnownSymbol (AlignedWordPrintfFormatBits a))

-- | Constructor for a proxy fot the promoted 'Align8', e.g. for
-- 'formatBits'.
align8 :: Proxy 'Align8
align8 = Proxy

-- | Constructor for a proxy fot the promoted 'Align16', e.g. for
-- 'formatBits'.
align16 :: Proxy 'Align16
align16 = Proxy

-- | Constructor for a proxy fot the promoted 'Align32', e.g. for
-- 'formatBits'.
align32 :: Proxy 'Align32
align32 = Proxy

-- | Constructor for a proxy fot the promoted 'Align64', e.g. for
-- 'formatBits'.
align64 :: Proxy 'Align64
align64 = Proxy

-- | Return an adequate alignment for records with @n@ bits.
type family SelectAlignment (n :: Nat) :: Maybe Alignment where
  SelectAlignment  x =
     If ((x `Rem` 64) == 0) ('Just 'Align64)
    (If ((x `Rem` 32) == 0) ('Just 'Align32)
    (If ((x `Rem` 16) == 0) ('Just 'Align16)
    (If ((x `Rem`  8) == 0) ('Just 'Align8)
       (TypeError ('Text "Bit record size is not divisable by 8: "
                   ':<>: 'ShowType x)))))

type family GetAlignmentBits (a :: Alignment) :: Nat where
  GetAlignmentBits 'Align8 = 8
  GetAlignmentBits 'Align16 = 16
  GetAlignmentBits 'Align32 = 32
  GetAlignmentBits 'Align64 = 64

-- | Calculate the number of unaligned bits with respect to a specific
-- 'Alignment'. This is just the type level integer remainder using the number
-- of bits in the alignment as returned by 'GetAlignmentBits'.
type family GetRemainingUnaligned (n :: Nat) (a :: Alignment) :: Nat where
  GetRemainingUnaligned n a = n `Rem` GetAlignmentBits a

type family MustFitInto (a :: Alignment) k :: Constraint where
  MustFitInto a t =
    If (FitsInto a t)
      ( 'True ~ 'True )
      (TypeError
        ('Text "Cannot fit " ':<>:
         'ShowType t ':<>:
         'Text " into " ':<>:
         'ShowType a))

type family FitsInto (a :: Alignment) k :: Bool where
  FitsInto 'Align64 Word64 = 'True
  FitsInto 'Align64 Word32 = 'True
  FitsInto 'Align64 Word16 = 'True
  FitsInto 'Align64 Word8 = 'True
  FitsInto 'Align64 Int64 = 'True
  FitsInto 'Align64 Int32 = 'True
  FitsInto 'Align64 Int16 = 'True
  FitsInto 'Align64 Int8 = 'True
  FitsInto 'Align64 _    = 'False
  FitsInto 'Align32 Word32 = 'True
  FitsInto 'Align32 Word16 = 'True
  FitsInto 'Align32 Word8 = 'True
  FitsInto 'Align32 Int32 = 'True
  FitsInto 'Align32 Int16 = 'True
  FitsInto 'Align32 Int8 = 'True
  FitsInto 'Align32 _    = 'False
  FitsInto 'Align16 Word16 = 'True
  FitsInto 'Align16 Word8 = 'True
  FitsInto 'Align16 Int16 = 'True
  FitsInto 'Align16 Int8 = 'True
  FitsInto 'Align16 _    = 'False
  FitsInto 'Align8 Word8 = 'True
  FitsInto 'Align8 Int8 = 'True
  FitsInto 'Align8 _    = 'False

type family ToAlignedWord (alignemnt :: Alignment) where
  ToAlignedWord 'Align64 = Word64
  ToAlignedWord 'Align32 = Word32
  ToAlignedWord 'Align16 = Word16
  ToAlignedWord 'Align8  =  Word8

-- * Endianness

-- | The endianness for the serialization of a 'BitBuffer'.
data Endianness = BigEndian | LittleEndian

-- | A 'Proxy' for the promoted type 'BigEndian', e.g. for
-- 'formatAlignedBits'.
bigEndian :: Proxy 'BigEndian
bigEndian = Proxy

-- | A 'Proxy' for the promoted type 'LittleEndian', e.g. for
-- 'formatAlignedBits'.
littleEndian :: Proxy 'LittleEndian
littleEndian = Proxy

-- * Bit Buffering

-- | Types which contain a finite amount of bits, which can be set from a value
-- and an offset. Bits can be written to the value.
class
  ( Num a, Show a, FiniteBits a, Bits a
  , KnownNat (BitBufferSize a))
  =>
  IsBitBuffer a where
    -- | IsBitBuffer a ,
    type BitBufferSize a :: Nat
    -- | Type level calculation for the rIsBitBuffer a r
    type CopyBitsRestLength a (len :: Nat) (offset :: Nat) :: Nat
    type CopyBitsRestLength a len offset = (len + offset) `Rem` BitBufferSize a
    -- | Copy bits starting at a specific offset from one @a@ the the other.
    copyBits
      :: Int  -- ^ @length@ of the value to write in number of bits.
      -> a -- ^ The value to write (in the lower @length@ bits).
      -> Int  -- ^ The start offset in the output value
      -> a -- ^ The input to write to
      -> (a, Int, Int, a) -- ^ The output buffer, space left in buffer, the
                          -- number of remaining bits that did not fit in the
                          -- buffer, and finally the left bits themselves.

-- | Return the static size of a 'IsBitBuffer'.
bitBufferSizeProxy :: forall b proxy . (IsBitBuffer b) => proxy b -> Int
bitBufferSizeProxy _ =
  fromIntegral $ natVal $ (Proxy :: Proxy (BitBufferSize b))

-- | Return the static size of an 'IsBitBuffer'. The parameter is ignored!
bitBufferSize :: forall b . (IsBitBuffer b) => b -> Int
bitBufferSize _ =
  fromIntegral $ natVal $ (Proxy :: Proxy (BitBufferSize b))

-- | A wrapper around an integral type retreived from 'ToAlignedWord' like
-- 'Word32', 'Word64', etc, that acts as a buffer for efficient serialization of
-- bits to a 'Builder'.
newtype BitBuffer (a :: Alignment) (endianness :: Endianness) =
  BitBuffer {fromBitBufferMsbFirst :: ToAlignedWord a}

deriving instance Eq (ToAlignedWord a) => Eq (BitBuffer a e)
deriving instance Ord (ToAlignedWord a) => Ord (BitBuffer a e)
deriving instance Num (ToAlignedWord a) => Num (BitBuffer a e)
deriving instance Bits (ToAlignedWord a) => Bits (BitBuffer a e)
deriving instance FiniteBits (ToAlignedWord a) => FiniteBits (BitBuffer a e)

instance ( KnownSymbol (AlignedWordPrintfFormatBits a)
         , PrintfArg (ToAlignedWord a))
         => Show (BitBuffer a e) where
  show (BitBuffer x) = printf fmt x
    where fmt = symbolVal (Proxy :: Proxy (AlignedWordPrintfFormatBits a))

type family AlignedWordPrintfFormatBits (a :: Alignment) :: Symbol where
  AlignedWordPrintfFormatBits 'Align64 = "%0.64b"
  AlignedWordPrintfFormatBits 'Align32 = "%0.32b"
  AlignedWordPrintfFormatBits 'Align16 = "%0.16b"
  AlignedWordPrintfFormatBits 'Align8 = "%0.8b"

instance ( KnownAlignment a, FiniteBits (BitBuffer a e) )
    => IsBitBuffer (BitBuffer a e) where
  type BitBufferSize (BitBuffer a e) = (GetAlignmentBits a)
  -- | Set bits starting from the most significant bit to the least.
  --   For example @writeBits m 1 <> writeBits n 2@ would result in:
  -- @
  --         MSB                                             LSB
  --    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
  --  Value: |0     ..     1|0        ..     10|  ...          |
  --          ->             ->                 ->     (direction of writing)
  -- @
  copyBits !len !bits !offset !buff =
    let buffLen = bitBufferSize buff
        spaceAvailable = buffLen - offset
        writeLen = min spaceAvailable len
        spaceLeft = spaceAvailable - writeLen
        writeOffset = spaceLeft
        restLen = len - writeLen
        restBits = bits .&. (1 `unsafeShiftL` restLen - 1)
        buff' = buff .|. (bits `unsafeShiftR` restLen `unsafeShiftL` writeOffset)
        in (buff', spaceLeft, restLen, restBits)

-- | Words acting as aligned bit buffers, that can eventually be converted to a
-- 'Builder'.
class ( IsBitBuffer a ) => HasBuilder a where
  wordBuilder :: a -> Builder

instance HasBuilder (BitBuffer 'Align64 'LittleEndian) where
  wordBuilder = word64LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align64 'BigEndian)    where
  wordBuilder = word64BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align32 'LittleEndian) where
  wordBuilder = word32LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align32 'BigEndian)    where
  wordBuilder = word32BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align16 'LittleEndian) where
  wordBuilder = word16LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align16 'BigEndian)    where
  wordBuilder = word16BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align8 e) where
  wordBuilder = word8    . fromBitBufferMsbFirst

------------------

formatBits
  :: forall proxy0 proxy1 proxy2 rec endianness buff alignment off
   . ( buff ~ BitBuffer alignment endianness
     , off ~ GetRemainingUnaligned (GetRecordSize rec) alignment
     , HasFormatter (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
  => proxy0 endianness
  -> proxy1 alignment
  -> proxy2 rec
  -> FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off)
formatBits _pEnd _pAlign pRec = runHoley toFormatter'
  where
    toFormatter' ::
      Holey
        (BitBuilder buff 0 off)
        (BitBuilder buff 0 off)
        (FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
    toFormatter' = toFormatter pRec

formatAlignedBits
  :: forall proxy0 proxy1 rec endianness buff alignment recSize off
   . ( 'Just alignment ~ SelectAlignment (GetRecordSize rec)
     , buff    ~ BitBuffer alignment endianness
     , FiniteBits (ToAlignedWord alignment)
     , HasBuilder buff
     , recSize ~ GetRecordSize rec
     , KnownNat recSize
     , off ~ GetRemainingUnaligned recSize alignment
     , KnownNat off
     , HasFormatter (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
  => proxy0 endianness
  -> proxy1 rec
  -> FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off)
formatAlignedBits _pEnd pRec = runHoley toFormatter'
  where
    toFormatter' ::
      Holey
        (BitBuilder buff 0 off)
        (BitBuilder buff 0 off)
        (FmtArg (BitBuilder buff 0 off) rec (BitBuilder buff 0 off))
    toFormatter' = toFormatter pRec


type HBitBuilder buff f t r a = Holey (BitBuilder buff f t) r a
type HFinalBitBuilder buff a = HBitBuilder buff 0 0 (BitBuilder buff 0 0) a


toBuilder :: Num buff => BitBuilder buff 0 0 -> Builder
toBuilder = appBitBuilder mempty

toFlushedBuilder :: (KnownNat off, Num buff, HasBuilder buff)
  => BitBuilder buff 0 off -> Builder
toFlushedBuilder bb = toBuilder (bb `ixAppend` flushBuilder)

flushBuilder :: forall buff off . (KnownNat off, HasBuilder buff)
  => BitBuilder buff off 0
flushBuilder =
    let flushBBState :: BBState buff off -> BBState buff 0
        flushBBState bb@(BBState bldr part) =
          let off = natVal bb
          in initialBBState $
              if off == 0
                then bldr
                else bldr <> wordBuilder part
    in  modifyBitBuilder flushBBState

appBitBuilder :: Num buff => Builder -> BitBuilder buff 0 0 -> Builder
appBitBuilder !b (BitBuilder !f) =
  bbStateBuilder (appIxEndo f (initialBBState b))

startBitBuilder :: Num buff => Builder -> BitBuilder buff n 0
startBitBuilder b = modifyBitBuilder (const (initialBBState b))

newtype BitBuilder buff
                   (fromOffset :: Nat)
                   (toOffset :: Nat)
  = BitBuilder (IxEndo (BBState buff) fromOffset toOffset)
  deriving IxMonoid

modifyBitBuilder
  :: (BBState buff fromOffset -> BBState buff toOffset)
  -> BitBuilder buff fromOffset toOffset
modifyBitBuilder = BitBuilder . IxEndo

data BBState buff (offset :: Nat) =
  BBState {  bbStateBuilder    :: !Builder
          , _bbStatePart       :: !buff}

instance (KnownNat o, Show buff) => Show (BBState buff o) where
  showsPrec d st@(BBState b p) =
    showParen (d > 10) $
          showString (printf "BBState %s" (printBuilder b))
        . (showChar ' ')
        . (showsPrec 11 p)
        . (showChar ' ')
        . (showsPrec 11 (natVal st))

printBuilder :: Builder -> String
printBuilder b =
      ("<< " ++)
   $  (++" >>")
   $  unwords
   $  printf "%0.2x"
  <$> (B.unpack $ toLazyByteString b)


initialBBState :: Num buff => Builder -> BBState buff 0
initialBBState b = BBState b 0

-- | Write all the bits, in chunks, filling and writing the 'BitBuffer'
-- in the 'BitBuilder' as often as necessary.
writeBits
      :: ( KnownNat len, HasBuilder buff
         , KnownNat fromOffset
         , Show buff
         , KnownNat toOffset
         , toOffset ~ CopyBitsRestLength buff len fromOffset)
      => proxy (len :: Nat)
      -> buff
      -> BitBuilder buff fromOffset toOffset
writeBits pLen !pBits =
  modifyBitBuilder $
    \bb@(BBState !bldr !part) ->
      go (fromIntegral (natVal pLen))
         pBits
         bldr
         part
         (fromIntegral (natVal bb))
  where
    go 0 _bits !bldr !part _ =  BBState bldr part
    go !len !bits !builder !part !offset =
      let (part', spaceLeft, restLen, restBits) = copyBits len bits offset part
          in if spaceLeft > 0
                then BBState builder part'
                else let nextBuilder = builder <> wordBuilder part'
                         in go restLen restBits nextBuilder 0 0

-------------------------

class HasFormatter m f r where
  type FmtArg m f r
  type FmtArg m f r = r
  toFormatter :: proxy f -> Holey m r (FmtArg m f r)

instance ( KnownNat oF, KnownNat oT, HasBuilder b
         , IsBitBuffer b, Bits b, KnownNat (GetRecordSize f)
         , oT ~ CopyBitsRestLength b (GetRecordSize f) oF)
  => HasFormatter (BitBuilder b oF oT) (l :=> f) r where
    type FmtArg (BitBuilder b oF oT) (l :=> f) r =
      Tagged l Integer -> r
    toFormatter _ =
        indirect (writeBits fieldLen . fromIntegral)
      where
        fieldLen = Proxy :: Proxy (GetRecordSize f)

instance  ( HasBuilder b
          , KnownNat oF, KnownNat oT
          , IsBitBuffer b
          , Bits b
          , KnownNat v
          , KnownNat (GetRecordSize f)
          , oT ~ CopyBitsRestLength b (GetRecordSize f) oF)
  => HasFormatter (BitBuilder b oF oT) (f := v) r where
    type FmtArg (BitBuilder b oF oT) (f := v) r = r
    toFormatter _ =
        immediate (writeBits fieldLen fieldVal)
      where
        fieldLen = Proxy :: Proxy (GetRecordSize f)
        fieldVal = fromIntegral (natVal (Proxy :: Proxy v))

-- | An instance that when given:
--
-- > type TwoFields = "f0" :=> Field m :>: "f1" :=> Field n
--
-- Writes:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: \------f0-----/\--------f1--------/\--- empty ---/
-- @
--
-- Where @k@ is the current bit offset.
-- The input values are expected to be in the order of the fields, i.e.:
--
-- @
-- runHoley $ toFormatter (Proxy :: Proxy TwoFields) 1 2
-- @
--
-- Will result in:
-- @       MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0       ..      10| X    ..      X|
-- @
instance forall f0 f1 a b bb oF o oT  .
         ( HasFormatter (BitBuilder bb oF o) f0 b
         , HasFormatter (BitBuilder bb o oT) f1 a
         , o ~ (CopyBitsRestLength bb (GetRecordSize f0) oF)
         , oT ~ (CopyBitsRestLength bb (GetRecordSize f1) o)
         , KnownNat oF, KnownNat o, KnownNat oT
         , IsBitBuffer bb
         , HasBuilder bb
         , b ~ FmtArg (BitBuilder bb o oT) f1 a)
  => HasFormatter (BitBuilder bb oF oT) (f0 :>: f1) a where
    type FmtArg (BitBuilder bb oF oT) (f0 :>: f1) a =
      FmtArg
        (BitBuilder bb oF (CopyBitsRestLength bb (GetRecordSize f0) oF))
        f0
        (FmtArg
          (BitBuilder bb (CopyBitsRestLength bb (GetRecordSize f0) oF) oT)
          f1
          a)
    toFormatter _ = fmt0 % fmt1
      where
        fmt0 :: Holey -- rely on ScopedTypeVariables and apply the types
                      -- so the compiler knows the result type of
                      -- toFormatter. Only then 'o' and
                      -- 'c ~ (FmtArg (BitBuilder bb oF oT) (f0 :>: f1) a)'
                      -- is known, yeah figure 'c' out ;)
                 (BitBuilder bb oF o)
                 (FmtArg (BitBuilder bb o oT) f1 a)
                 (FmtArg (BitBuilder bb oF oT) (f0 :>: f1) a)
        fmt0 = toFormatter pf0
        fmt1 = toFormatter pf1
        pf0 = Proxy :: Proxy f0
        pf1 = Proxy :: Proxy f1

-------------------------------------------------------------

newtype Holey m r a = HM {runHM :: ((m -> r) -> a) }

-- * Indexec Holey

class IxMonoid (m :: k -> k -> Type)  where
  ixEmpty :: m i i
  ixAppend :: m h i -> m i j -> m h j

newtype IxEndo (a :: k -> Type) (i :: k) (j:: k) where
  IxEndo :: { appIxEndo :: a i -> a j } -> IxEndo a i j

instance IxMonoid (IxEndo (a :: k -> Type)) where
  ixEmpty = IxEndo id
  ixAppend (IxEndo f) (IxEndo g) = IxEndo (g . f)

(%) :: IxMonoid (m :: k ->  k -> Type) =>
  Holey (m h i) b c ->
  Holey (m i j) a b ->
  Holey (m h j) a c
(%) (HM f) (HM g) =
  HM (\k -> (f (\m1 -> g (\m2 -> k (m1 `ixAppend` m2)))))

-- * Normal Holey

instance Monoid m => Category (Holey m) where
  (.) (HM f) (HM g) = HM (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))
  id = HM ($ mempty)

hoistM :: (m -> n) -> Holey m a b -> Holey n a b
hoistM into (HM f) = HM (\k -> f (k . into))

hoistR :: (s -> r) -> Holey m r a -> Holey m s a
hoistR outof (HM f) = HM (\k -> f (outof . k))

immediate :: m -> Holey m r r
immediate m =
  HM { runHM = ($ m) }

indirect :: (a -> m) -> Holey m r (a -> r)
indirect f =
  HM { runHM = (. f) }


bind :: Holey m b c
      -> (m -> Holey n a b)
      -> Holey n a c
bind mbc fm = HM $ \ kna -> runHM mbc (($ kna) . runHM . fm)

runHoley :: Holey m m a -> a
runHoley = ($ id) . runHM
