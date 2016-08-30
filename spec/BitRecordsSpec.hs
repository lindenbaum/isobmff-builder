{-# LANGUAGE UndecidableInstances #-}
module BitRecordsSpec (spec) where

import Data.Bits
import Data.ByteString.Builder
import Data.Proxy
import Data.Tagged
import Data.Type.BitRecords
import Data.Type.Equality ()
import Data.Word
import GHC.TypeLits
import Prelude hiding ((.), id)
import Test.Hspec
import Test.QuickCheck (property)
import Test.TypeSpecCrazy
import Text.Printf

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
          -- -*  0 `ShouldBe` BitRecordSize 'Nothing
          -- -*  32 `ShouldBe` BitRecordSize ('Just Word32)
          -- -*  0 `ShouldBe` BitRecordSize '[]
          -- -*  10 `ShouldBe` BitRecordSize '[Field 10]
          -- -*  25 `ShouldBe` BitRecordSize '[Field 10, Field 15]
          -- -*  1 `ShouldBe` BitRecordFieldSize (ToBitRecordField Bool)
          -- -*  1 `ShouldBe` BitRecordFieldSize (ToBitRecordField 'True)
          -- -*  1 `ShouldBe` BitRecordFieldSize (ToBitRecordField 'False)
        checkFlagJust = Valid
    runIO $ print checkFlagJust
  describe "bitStringBuilder" $ do
    describe "Just x" $ it "writes x" $
      bitStringPrinter (Proxy :: Proxy (ToBitRecord ('Just (Flag := 'True)))) `shouldBe` "<< 80 >>"
    describe "Nothing" $ it "writes nothing" $
      bitStringPrinter (Proxy :: Proxy (ToBitRecord ('Nothing))) `shouldBe` "<<  >>"
    describe "'[]" $ it "writes nothing" $
      bitStringPrinter (Proxy :: Proxy (ToBitRecord ('[]))) `shouldBe` "<<  >>"
    describe "'[x1, x2]" $ it "writes x1 then x2" $
      bitStringPrinter (Proxy :: Proxy (ToBitRecord ('[FieldU8 := 1, FieldU8 := 2]))) `shouldBe` "<< 01 02 >>"
    describe "'True" $ it "writes a single bit with a 1" $
      bitStringPrinter (Proxy :: Proxy (ToBitRecord 'True)) `shouldBe` "<< 80 >>"
    describe "'False" $ it "writes a single bit with a 0" $
      bitStringPrinter (Proxy :: Proxy (ToBitRecord 'False)) `shouldBe` "<< 00 >>"
    describe "Bool" $ it "writes a single bit from a tagged 'Bool' parameter" $
      property $ \ v ->
        let actual = if v then "<< 80 >>" else "<< 00 >>" in
        actual `shouldBe` bitStringPrinter (Proxy :: Proxy (ToBitRecord ("x" :=> Flag))) (Tagged v)
    describe "FlagJust" $ do
      it "writes a single bit '1' for a 'Just ...' parameter" $
        bitStringPrinter (Proxy :: Proxy (FlagJust ('Just "test"))) `shouldBe` "<< 80 >>"
      it "writes a single bit '0' for a 'Nothing' parameter" $
        bitStringPrinter (Proxy :: Proxy (FlagJust 'Nothing)) `shouldBe` "<< 00 >>"
    describe "FlagNothing" $ do
      it "writes a single bit '0' for a 'Just ...' parameter" $
        bitStringPrinter (Proxy :: Proxy (FlagNothing ('Just "test"))) `shouldBe` "<< 00 >>"
      it "writes a single bit '1' for a 'Nothing' parameter" $
        bitStringPrinter (Proxy :: Proxy (FlagNothing 'Nothing)) `shouldBe` "<< 80 >>"
  describe "showRecord" $ do
    describe "Maybe" $ do
      it "prints 'Just x' as 'x'" $
        showRecord (Proxy :: Proxy (ToBitRecord ('Just 'True))) `shouldBe` "T"
      it "prints nothing for 'Nothing'" $
        showRecord (Proxy :: Proxy (ToBitRecord ('Nothing))) `shouldBe` ""
    describe "List" $ do
      it "prints '[x]' as 'x'" $
        showRecord (Proxy :: Proxy (ToBitRecord ('[ 'True ]))) `shouldBe` "T"
      it "prints '[x1, x2, x3]' as 'x1x2x3'" $
        showRecord (Proxy :: Proxy (ToBitRecord ('[ Bool, Bool, Bool ]))) `shouldBe` "BBB"
      it "prints nothing for '[]'" $
        showRecord (Proxy :: Proxy (ToBitRecord ('[]))) `shouldBe` ""
    describe "Bool" $ do
      it "prints a 'T' if the parameter is 'True" $
        showRecord (Proxy :: Proxy (ToBitRecord ('True))) `shouldBe` "T"
      it "prints a 'F' if the parameter is 'False" $
        showRecord (Proxy :: Proxy (ToBitRecord ('False))) `shouldBe` "F"
      it "prints a 'B' if the parameter is Bool" $
        showRecord (Proxy :: Proxy (ToBitRecord Bool)) `shouldBe` "B"
    describe "FlagJust" $ do
      it "prints a 'T' if the parameter is 'Just ..'" $
        showRecord (Proxy :: Proxy (ToBitRecord (FlagJust ('Just "123")))) `shouldBe` "T"
      it "prints a 'F' if the parameter is 'Nothing'" $
        showRecord (Proxy :: Proxy (ToBitRecord (FlagJust 'Nothing))) `shouldBe` "F"
    describe "FlagNothing" $ do
      it "prints a 'F' if the parameter is 'Just ..'" $
        showRecord (Proxy :: Proxy (ToBitRecord (FlagNothing ('Just "123")))) `shouldBe` "F"
      it "prints a 'T' if the parameter is 'Nothing'" $
        showRecord (Proxy :: Proxy (ToBitRecord (FlagNothing 'Nothing))) `shouldBe` "T"

arraySpec =
    describe "RecArray" $ do
      describe "level record accessors" $ do
        let checkArrayRec ::
              "BitRecord accessors involving RecArray"
              #######################################

              "The record size works"
              ~~~~~~~~~~~~~~~~~~~~~~~~
                  1 `ShouldBe` BitRecordSize (ToBitRecord (RecArray Flag 1))
              -* 91 `ShouldBe` BitRecordSize (ToBitRecord (("foo" :=> Flag :>: FieldU8) ^^ 10 :>: Flag))
              -* 91 `ShouldBe` BitRecordSize (ToBitRecord (RecArray ("foo" :=> Flag :>: FieldU8) 10 :>: Flag))
            checkArrayRec = Valid
        runIO $ print checkArrayRec
      describe "showRecord" $
        it "appends its body n times" $
        let expected = "<<utf-8[5]: hello>><<utf-8[5]: hello>><<utf-8[5]: hello>><<utf-8[5]: hello>><<utf-8[5]: hello>>"
            actual = showRecord (Proxy :: Proxy ( [utf8|hello|] ^^ 5))
            in actual `shouldBe` expected
      describe "bitStringBuilder" $
        it "writes its contents n times to the builder" $
          let actual = bitStringPrinter (Proxy :: Proxy (ToBitRecord ((Field 24 := 0x010203) ^^ 4)))
              expected = "<< 01 02 03 01 02 03 01 02 03 01 02 03 >>"
              in actual `shouldBe` expected

sizedSpec =
  describe "Sized" $ do
    describe "TypeChecks" $
      let
          checkSized ::
             "Sized"
            #########

            "BitRecordFieldSize (of) a SizeField"
            ~~~~~~~~~~~~~~~~~~~~~~~~~~~
                0 `ShouldBe` BitRecordFieldSize (ToBitRecordField 'NoSizeField)
            -*  8 `ShouldBe` BitRecordFieldSize (ToBitRecordField 'SizeField8)
            -* 16 `ShouldBe` BitRecordFieldSize (ToBitRecordField 'SizeField16)
            -* 32 `ShouldBe` BitRecordFieldSize (ToBitRecordField 'SizeField32)

            -- -/-

            -- "SizedString"
            -- ~~~~~~~~~~~~~~~
            --     88 `ShouldBe` BitRecordFieldSize [utf8|Hello World|]
            -- -* 104 `ShouldBe` BitRecordFieldSize [utf8|Heλλo World|]

            -- -/-

            -- "Sized of a list"
            -- ~~~~~~~~~~~~~~~~~~
            --     0 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'NoSizeField '[]))
            -- -*  3 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'NoSizeField '[Field 3]))
            -- -*  6 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'NoSizeField '[Field 3, Field 3]))
            -- -*  8 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField8 '[]))
            -- -* 11 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField8 '[Field 3]))
            -- -* 14 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField8 '[Field 3, Field 3]))
            -- -* 16 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField16 '[]))
            -- -* 19 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField16 '[Field 3]))
            -- -* 22 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField16 '[Field 3, Field 3]))
            -- -* 32 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField32 '[]))
            -- -* 35 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField32 '[Field 3]))
            -- -* 38 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField32 '[Field 3, Field 3]))

            -- -/-

            -- "Sized of a RecArray"
            -- ~~~~~~~~~~~~~~~~~~~~~
            --     0 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'NoSizeField (RecArray Word16 0)))
            -- -* 16 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'NoSizeField (RecArray Word16 1)))
            -- -* 32 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'NoSizeField (RecArray Word16 2)))
            -- -* 32 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField32 (RecArray Word16 0)))
            -- -* 48 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField32 (RecArray Word16 1)))
            -- -* 64 `ShouldBe` BitRecordSize (ToBitRecord (Sized 'SizeField32 (RecArray Word16 2)))

          checkSized = Valid
      in runIO $ print checkSized
    describe "showRecord" $ do
      describe "SizedString" $
        it "renders a string containing wide utf-8 characters to a header containing the number of chars and the actual string" $
        showRecord (Proxy :: Proxy [utf8|Heλλo World!|]) `shouldBe` "<<utf-8[12]: Heλλo World!>>"
      describe "Sized (SizeField16 SizedString)" $
        it "renders the number bytes not chars as the size field value" $
        showRecord (Proxy :: Proxy (Sized 'SizeField16 [utf8|Heλλo World!|])) `shouldBe` "000c: <<utf-8[12]: Heλλo World!>>"
    describe "bitStringBuilder" $ do
      describe "no length prefix" $
        it "renders no size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy [utf8|ABC|])
        `shouldBe`
        "<< 41 42 43 >>"
      describe "8-bit length prefix" $
        it "renders a single byte size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (Sized 'SizeField8 [utf8|ABC|]))
        `shouldBe`
        "<< 03 41 42 43 >>"
      describe "16-bit length prefix" $
        it "renders a big endian 16 bit size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (Sized 'SizeField16 [utf8|ABC|]))
        `shouldBe`
        "<< 00 03 41 42 43 >>"
      describe "32-bit length prefix" $
        it "renders a big endian 32 bit size prefix and the string as UTF-8 bytes" $
        bitStringPrinter (Proxy :: Proxy (Sized 'SizeField32 [utf8|ABC|]))
        `shouldBe`
        "<< 00 00 00 03 41 42 43 >>"

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
  :: Expect '[ ShouldBe 96        (BitRecordSize TestRecAligned)
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
  :: Expect '[ ShouldBe 71        (BitRecordSize TestRecUnAligned)
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

testAlign
  :: Expect '[  Align 'True 7 (ToBitRecord Flag)       `ShouldBe`  (Flag :>: Field 6 := 0)
             , Align 'True 1 (ToBitRecord Flag)       `ShouldBe`  (ToBitRecord Flag)
             , Align 'True 8 (ToBitRecord (Field 7))  `ShouldBe`  (Field 7 :>: Field 1 := 0)
             , Align 'True 8 (ToBitRecord (Field 8))  `ShouldBe`  (ToBitRecord (Field 8))
             , Align 'True 8 (ToBitRecord (Field 9))  `ShouldBe`  (Field 9 :>: Field 7 := 0)
            ]
testAlign = Valid

type TestField0 =  ToBitRecord ("test" :=> Field 19)
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

spec :: Spec
spec = do
  sizedSpec
  basicsSpec
  arraySpec
  describe "The Set of Type Functions" $
    it "is sound" $ do
      print (Valid :: Expect (BitRecordSize (Flag :>: Field 7) `Is` 8))
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
  describe "showRecord" $ do
    it "prints (Field 4 :>: (Field 4 := 0x96)) to \"<..>0110\"" $
      let actual = showRecord (Proxy :: Proxy (Field 4 :>: (Field 4 := 0x96)))
          in actual `shouldBe` "????0110"
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
  describe "StaticLazybytestringbuilder" $ do
    it "writes (and flushes) bits" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB =
                  runBitStringBuilderHoley
                  (bitStringBuilderHoley rec)
                          1 -- because instance Num a => Num (Tagged t a)
                          (Tagged 3 :: Tagged "baz" Word64)
                          (Tagged 7 :: Tagged "foo" Word64)
            actual = printBuilder actualB
            in  actual `shouldBe`
                  "<< 01 00 06 00 00 00 00 0f fc >>"
    describe "Formatting sub-byte fields" $ do
      it "only the addressed bits are copied to the output" $
        property $ \value ->
          let rec = Proxy
              rec :: Proxy (Field 4 := 0 :>: "here" :=> Field 4)
              actualB :: Builder
              actualB = runBitStringBuilderHoley (bitStringBuilderHoley rec)
                          (Tagged value :: Tagged "here" Word64)
              actual = printBuilder actualB
              expected = printf "<< %.2x >>" (value .&. 0xf)
              in actual `shouldBe` expected
      it "renders (Flag := 0 :>: (Field 7 := 130)) to << 02 >>" $
        let rec = Proxy
            rec :: Proxy (Flag := 'False :>: (Field 7 := 130))
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
-- * Bit Buffering
