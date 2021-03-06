{-# LANGUAGE UndecidableInstances #-}
module BitRecordsSpec (spec) where

import Data.Bits
import Data.ByteString.Builder
import Data.Proxy
import Data.Type.BitRecords
import Data.Type.Equality ()
import Data.Kind.Extra
import GHC.TypeLits
import Prelude hiding ((.), id)
import Test.Hspec
import Test.QuickCheck (property, Arbitrary(..), choose)
import Test.TypeSpecCrazy
import Text.Printf

basicsSpec :: Spec
basicsSpec = do
  describe "Maybe, Lists, Either, Bool, 'True, 'False, FlagJust, FlagNothing" $ do
    let checkFlagJust ::
          "Type Level Bool, 'True, 'False, FlagJust, FlagNothing Accessors"
          ##################################################################

          "The record size works"
          ~~~~~~~~~~~~~~~~~~~~~~~~
              1 `ShouldBe` BitRecordFieldSize (FlagJust 'Nothing)
          -*  1 `ShouldBe` BitRecordFieldSize (FlagJust ('Just "Blah"))
          -*  1 `ShouldBe` BitRecordFieldSize (FlagNothing ('Just "Blah"))
          -- TODO reenable tests
          -- -*  32 `ShouldBe` BitRecordSize (ToBitRecord ('Just FieldU32))
          -- -*  0 `ShouldBe` BitRecordSize (ToBitRecord '[])
          -- -*  10 `ShouldBe` BitRecordSize (ToBitRecord '[Field 10])
          -- -*  25 `ShouldBe` BitRecordSize (ToBitRecord '[Field 10, Field 15])
          -- -*  1 `ShouldBe` BitRecordSize (ToBitRecord Bool)
          -- -*  1 `ShouldBe` BitRecordSize (ToBitRecord 'True)
          -- -*  1 `ShouldBe` BitRecordSize (ToBitRecord 'False)
        checkFlagJust = Valid
    runIO $ print checkFlagJust
  describe "bitStringBuilder" $ do
    describe "Just x" $ it "writes x" $
      bitStringPrinter (Proxy :: Proxy (OptionalRecord ('Just ('BitRecordMember (Flag := 'True))))) `shouldBe` "<< 80 >>"
    describe "Nothing" $ it "writes nothing" $
      bitStringPrinter (Proxy :: Proxy (OptionalRecord ('Nothing))) `shouldBe` "<<  >>"
    -- describe "'[]" $ it "writes nothing" $
    --   bitStringPrinter (Proxy :: Proxy (BitRecordOfList (Fun1 RecordField)  ('[]))) `shouldBe` "<<  >>"
    -- describe "'[x1, x2]" $ it "writes x1 then x2" $
    --   bitStringPrinter (Proxy :: Proxy (BitRecordOfList (Fun1 RecordField) ('[FieldU8 := 1, FieldU8 := 2]))) `shouldBe` "<< 01 02 >>"
    describe "'True" $ it "writes a single bit with a 1" $
      bitStringPrinter (Proxy :: Proxy (RecordField (Flag := 'True))) `shouldBe` "<< 80 >>"
    describe "'False" $ it "writes a single bit with a 0" $
      bitStringPrinter (Proxy :: Proxy (RecordField (Flag := 'False))) `shouldBe` "<< 00 >>"
    describe "@: labelled fields"  $ do
      it "writes them ..." $
        let fld = Proxy @(Eval (RecordField ( "foo" @: FlagJust 'Nothing  )))
        in bitStringPrinter fld `shouldBe` "<< 00 >>"
    describe "FlagJust" $ do
      it "writes a single bit '1' for a 'Just ...' parameter" $
        bitStringPrinter (Proxy :: Proxy (RecordField (FlagJust ('Just "test")))) `shouldBe` "<< 80 >>"
      it "writes a single bit '0' for a 'Nothing' parameter" $
        bitStringPrinter (Proxy :: Proxy (RecordField (FlagJust 'Nothing))) `shouldBe` "<< 00 >>"
    describe "FlagNothing" $ do
      it "writes a single bit '0' for a 'Just ...' parameter" $
        bitStringPrinter (Proxy :: Proxy (RecordField (FlagNothing ('Just "test")))) `shouldBe` "<< 00 >>"
      it "writes a single bit '1' for a 'Nothing' parameter" $
        bitStringPrinter (Proxy :: Proxy (RecordField (FlagNothing 'Nothing))) `shouldBe` "<< 80 >>"
  -- TODO reenable showRecord tests
  -- describe "showRecord" $ do
  --   describe "Maybe" $ do
  --     it "prints 'Just x'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord ('Just 'True))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'True"
  --     it "prints nothing for 'Nothing'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord ('Nothing))) `shouldBe` ""
  --   describe "List" $ do
  --     it "prints '[x]'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord ('[ 'True ]))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'True"
  --     it "prints '[x1, x2, x3]'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord ('[ Bool, Bool, Bool ]))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\nField:\n  Demote Rep: Bool\n  Bits: 1\nField:\n  Demote Rep: Bool\n  Bits: 1"
  --     it "prints nothing for '[]'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord ('[]))) `shouldBe` ""
  --   describe "Bool" $ do
  --     it "prints 'True" $
  --       showRecord (Proxy :: Proxy (ToBitRecord ('True))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'True"
  --     it "prints 'False" $
  --       showRecord (Proxy :: Proxy (ToBitRecord ('False))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'False"
  --     it "prints a Bool" $
  --       showRecord (Proxy :: Proxy (ToBitRecord Bool)) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1"
  --   describe "FlagJust" $ do
  --     it "prints a 'FlagJust 'Just ..'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord (FlagJust ('Just "123")))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'True"
  --     it "prints a 'FlagJust 'Nothing'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord (FlagJust 'Nothing))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'False"
  --   describe "FlagNothing" $ do
  --     it "prints 'FlagNothing 'Just ..'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord (FlagNothing ('Just "123")))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'False"
  --     it "prints a 'FlagNothing 'Nothing'" $
  --       showRecord (Proxy :: Proxy (ToBitRecord (FlagNothing 'Nothing))) `shouldBe` "Field:\n  Demote Rep: Bool\n  Bits: 1\n  Static Value: 'True"

#ifdef COMPLEXTESTS
arraySpec :: SpecWith ()
arraySpec =
    describe "RecArray" $ do
      describe "level record accessors" $ do
        let checkArrayRec ::
              "BitRecord accessors involving RecArray"
              #######################################

              "The record size works"
              ~~~~~~~~~~~~~~~~~~~~~~~~
                  1 `ShouldBe` BitRecordSize (Eval (RecArray ('BitRecordMember Flag) 1))
              -* 91 `ShouldBe` BitRecordSize (Eval (("foo" @: Flag .+. FieldU8) ^^ 10) :+. Flag)
              -* 91 `ShouldBe` BitRecordSize (Eval (RecArray ("foo" @: Flag .+. FieldU8) 10) :+. Flag)
            checkArrayRec = Valid
        runIO $ print checkArrayRec
      describe "showRecord" $
        it "appends its body n times" $
        let expected = "utf-8(40) := <<hello>> [5 Bytes]\nutf-8(40) := <<hello>> [5 Bytes]\nutf-8(40) := <<hello>> [5 Bytes]\nutf-8(40) := <<hello>> [5 Bytes]\nutf-8(40) := <<hello>> [5 Bytes]"
            actual = showARecord (Proxy @ (('BitRecordMember [utf8|hello|] ^^ 5)))
            in actual `shouldBe` expected
      describe "bitStringBuilder" $
        it "writes its contents n times to the builder" $
          let actual = bitStringPrinter (Proxy :: Proxy (('BitRecordMember (Field 24 := 0x010203) ^^ 4)))
              expected = "<< 01 02 03 01 02 03 01 02 03 01 02 03 >>"
              in actual `shouldBe` expected

sizedSpec =
  describe "Sized" $ do
    describe "TypeChecks" $
      let
          checkSized ::
             "Sized"
            #########

            "SizedString"
            ~~~~~~~~~~~~~~~
                88 `ShouldBe` BitRecordFieldSize [utf8|Hello World|]
            -* 104 `ShouldBe` BitRecordSize (Eval (RecordField [utf8|Heλλo World|]))

            -/-

            "Sized BitRecord Members"
            ~~~~~~~~~~~~~~~~~~~~~~~~
                8 `ShouldBe` BitRecordSize (Eval (Sized8 'EmptyBitRecord))
            -*  9 `ShouldBe` BitRecordSize (Eval (Sized8 ('BitRecordMember Flag)))
            -*  0 `ShouldBe` SizeFieldValue 'EmptyBitRecord
            -*  1 `ShouldBe` SizeFieldValue ('BitRecordMember Flag)

            -/-

            "SizedField"
            ~~~~~~~~~~~~
                9 `ShouldBe` BitRecordSize (Eval (SizedField8 Flag))
            -*  1 `ShouldBe` SizeFieldValue  Flag

            -- TODO add more Sized tests, especially for SizedField
          checkSized = Valid
      in runIO $ print checkSized
    describe "showRecord" $ do
      describe "SizedString" $
        it "renders a string containing wide utf-8 characters to a header containing the number of chars and the actual string" $
        showARecord (Proxy :: Proxy (RecordField [utf8|Heλλo World!|])) `shouldBe` "utf-8(112) := <<He\955\955o World!>> [14 Bytes]"
      describe "Sized SizeField16 SizedString" $
        it "renders the number bytes not chars as the size field value" $
        showARecord (Proxy :: Proxy (SizedField16 [utf8|Heλλo World!|])) `shouldBe` "size: U16 := hex: 000e (dec: 14)\nutf-8(112) := <<He\955\955o World!>> [14 Bytes]"
    describe "bitStringBuilder" $ do
      describe "no length prefix" $
        it "renders no size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (RecordField [utf8|ABC|]))
        `shouldBe`
        "<< 41 42 43 >>"
      describe "8-bit length prefix" $
        it "renders a single byte size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (SizedField8 [utf8|ABC|]))
        `shouldBe`
        "<< 03 41 42 43 >>"
      describe "16-bit length prefix" $
        it "renders a big endian 16 bit size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (SizedField16 [utf8|ABC|]))
        `shouldBe`
        "<< 00 03 41 42 43 >>"
      describe "32-bit length prefix" $
        it "renders a big endian 32 bit size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (SizedField32 [utf8|ABC|]))
        `shouldBe`
        "<< 00 00 00 03 41 42 43 >>"
      describe "64-bit length prefix" $
        it "renders a big endian 64 bit size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (SizedField64 [utf8|ABC|]))
        `shouldBe`
        "<< 00 00 00 00 00 00 00 03 41 42 43 >>"

type TestRecAligned =
  "bar" @: Field 8       .+:
            Field 8  := 0 .+:
  "baz" @: Field 8       .+:
            Field 32 := 0 .+:
  "foo" @: Field 8       .+:
            Field 8  := 0 .+:
  "oof" @: Field 8       .+:
            Field 8  := 0 .+.
  "rab" @: Field 8

checkTestRecAligned
  :: Expect '[ ShouldBe 96        (BitRecordSize TestRecAligned)  ]
checkTestRecAligned = Valid

type TestRecUnAligned =
  "bar" @: Field 8       .+:
            Field 8  := 0 .+:
  "baz" @: Field 7       .+:
            Field 32 := 0 .+:
  "foo" @: Field 8       .+.
            Field 8  := 0xfe

checkTestRecUnAligned
  :: Expect '[ ShouldBe 71        (BitRecordSize TestRecUnAligned) ]
checkTestRecUnAligned = Valid

testTakeLastN ::
  "Taking the last n elements of a list" #######################################

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
#endif


spec :: Spec
spec = do
  basicsSpec
#ifdef COMPLEXTESTS
  sizedSpec
  arraySpec
  describe "The Set of Type Functions" $
    it "is sound" $ do
      print (Valid :: Expect (BitRecordSize (Flag .+. Field 7) `Is` 8))
      print testTakeLastN
      print testRem
      print testRemPow2
      print testDiv
      print testNatBits
      print checkTestRecAligned
      print checkTestRecUnAligned
  describe "showARecord" $ do
    it "prints (Field 4 .+. (Field 4 := 0x96)) to \"<..>0110\"" $
      let actual = showRecord (Proxy :: Proxy (Field 4 .+. Field 4 := 0x96))
          in actual `shouldBe` "bits(4)\nbits(4) := 10010110 (hex: 96 dec: 150)"
  describe "StaticLazybytestringbuilder" $ do
    it "writes (and flushes) bits" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB =
                  runBitStringBuilderHoley
                  (bitStringBuilderHoley rec)
                          1
                          3
                          7
            actual = printBuilder actualB
            in  actual `shouldBe`
                  "<< 01 00 06 00 00 00 00 0f fc >>"
    describe "Formatting sub-byte fields" $ do
      it "only the addressed bits are copied to the output" $
        property $ \value ->
          let rec = Proxy
              rec :: Proxy (Field 4 := 0 .+. "here" @: Field 4)
              actualB :: Builder
              actualB = runBitStringBuilderHoley (bitStringBuilderHoley rec) value
              actual = printBuilder actualB
              expected = printf "<< %.2x >>" (value .&. 0xf)
              in actual `shouldBe` expected
      it "renders (Flag := 0 .+. Field 7 := 130) to << 02 >>" $
        let rec = Proxy
            rec :: Proxy (Flag := 'False .+. Field 7 := 130)
            actual = printBuilder b
              where b = runBitStringBuilderHoley (bitStringBuilderHoley rec)
        in actual `shouldBe` "<< 02 >>"
  describe "ByteStringBuilder" $
    describe "runBitStringBuilderHoley" $
      it "0x01020304050607 to << 00 01 02 03 04 05 06 07 >>" $
        let expected = "<< 00 01 02 03 04 05 06 07 >>"
            actual =
               printBuilder
                (runBitStringBuilderHoley
                 (bitStringBuilderHoley
                     (bitString 64 0x01020304050607)))
            in actual `shouldBe` expected
#endif

instance (KnownNat n, n <= 64) => Arbitrary (B n) where
  arbitrary = do
    let h = 2^(n'-1) - 1
        n' = fromIntegral (natVal (Proxy @n)) :: Int
    x <- choose (0, h + h + 1)
    return (B x)
