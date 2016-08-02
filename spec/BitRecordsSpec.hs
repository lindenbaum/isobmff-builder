module BitRecordsSpec (spec) where

import Data.Bits
import Data.ByteString.IsoBaseFileFormat.Util.BitRecords
import Data.Proxy
import Data.Word
import GHC.TypeLits
import Test.Hspec
import Test.TypeSpecCrazy


spec :: Spec
spec = do
  describe "The Set of Type Functions" $
    it "is sound" $ do
      print (Valid :: Expect (GetFieldSize (Flag :*: Field 7) `Is` 8))
      print testBitHasFields
      print testBitHasNestedFields
      print testFocus
      print testFocusError
      print testRem
      print testAlign
      print testFieldPosition0
      print testFieldPosition1Foo
      print testFieldPosition1Bar
      print testFieldPosition1Baz
      print testFieldPositionTestFieldNested
      print testFieldPositionToList
  describe "Field-Getter" $ do
    it "returns False for flag 'foo'" $
      getFlag
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy ("no" :=> Flag :*: "foo" :=> Flag ))
        (0xd :: Word8)
       `shouldBe` False
    it "returns True for flag 'foo'" $
      getFlag
         (Proxy :: Proxy "foo")
         (Proxy :: Proxy ("no" :=> Flag :*: "foo" :=> Flag ))
         (0xf :: Word8)
       `shouldBe` True
    it "returns 0x1 for field 'foo'" $
      getField
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy (Field 16 :*: "foo" :=> Flag))
        (0x00010000 :: Word32)
       `shouldBe` (1 :: Word32)
    it "returns 0xcafe for field 'foo'" $
      getField
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy (Field 13 :*: "foo" :=> Field 16))
        (0xcafe0000 `shiftR` 3 :: Word32)
       `shouldBe` (0xcafe :: Word32)
    it "returns 0xcafe for field 'foo'" $
      getField
        (Proxy :: Proxy "foo")
        (Proxy :: Proxy (Field 13
                         :*: "foo" :=> (    Field 7
                                        :*: Flag
                                        :*: Field 6
                                        :*: Flag
                                        :*: Flag)))
        (0xcafe0000 `shiftR` 3 :: Word32)
       `shouldBe` (0xcafe :: Word32)
    it "returns 0xcafe for nested field 'bar :/ foo'" $
      getField
        (Proxy :: Proxy ("bar" :/ "foo"))
        (Proxy :: Proxy (              Field 13
                         :*: "bar" :=> (              Field 7
                                        :*:           Flag
                                        :*: "foo" :=> Field 16
                                        :*:           Flag
                                        :*:           Flag)))
        (0xcafe `shiftL` (13 + 7 + 1) :: Word64)
       `shouldBe` (0xcafe :: Word64)
  describe "Field-Setter" $ do
    it "sets nested flag 'bar :/ foo' to 1" $
      setFlag
        (Proxy :: Proxy ("bar" :/ "foo"))
        (Proxy :: Proxy (              Field 13
                         :*: "bar" :=> (              Field 7
                                        :*:           Flag
                                        :*: "foo" :=> Flag
                                        :*:           Flag
                                        :*:           Flag)))
        True
        (0 :: Word64)
       `shouldBe` (1 `shiftL` (13 + 7 + 1) :: Word64)
    it "sets nested field 'bar :/ foo' to 0xe" $
      setField
        (Proxy :: Proxy ("bar" :/ "foo"))
        (Proxy :: Proxy (              Field 13
                         :*: "bar" :=> (              Field 7
                                        :*:           Flag
                                        :*: "foo" :=> Field 4
                                        :*:           Flag
                                        :*:           Flag)))
        (0xe :: Word8)
        (0xcaf0 `shiftL` (13 + 7 + 1) :: Word64)
       `shouldBe` (0xcafe `shiftL` (13 + 7 + 1) :: Word64)


--

type TestHasField =
       "foo" :=> Flag
   :*:           Field 4
   :*: "bar" :=> Field 2
testBitHasFields
  :: Expect '(ShouldBeTrue (HasField TestHasField "foo"),
              ShouldBeTrue (HasField TestHasField "bar"))
testBitHasFields = Valid

type TestHasNestedField =
       "foo" :=> "bar" :=> Field 2
testBitHasNestedFields
  :: Expect (ShouldBeTrue (HasField TestHasNestedField ("foo" :/ "bar")))
testBitHasNestedFields = Valid

--

type TestFocus =
       "bad" :=> Field 13
   :*: "bar" :=> (              Field 7
                  :*: "bax" :=> Flag )
   :*: "foo" :=> ExpectedFocus

type ExpectedFocus =
  (     "fmm" :=> Flag
                  :*:           Flag
                  :*:           Flag)

testFocus
  :: Expect (FocusOnUnsafe "foo" TestFocus `Is` ExpectedFocus)
testFocus = Valid

type ExpectedFocusError =
  'Left
   (       'Text "Label not found. Cannot focus '"
     ':<>: 'ShowType "xxx"
     ':<>: 'Text "' in:"
     ':$$: 'ShowType
            (      "bad" :=> Field 13
               :*: "bar" :=> (Field 7 :*: "bax" :=> Flag)
               :*: "foo" :=> ExpectedFocus))

testFocusError
  :: Expect (FocusOn "xxx" TestFocus `ShouldBe` ExpectedFocusError)
testFocusError = Valid

--

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

--

testAlign
  :: Expect '[ Try (AlignField 7 Flag)       `ShouldBe`  (Flag :*: Field 6)
             , Try (AlignField 1 Flag)       `ShouldBe`  Flag
             , Try (AlignField 8 (Field 7))  `ShouldBe`  (Field 7 :*: Field 1)
             , Try (AlignField 8 (Field 8))  `ShouldBe`  Field 8
             , Try (AlignField 8 (Field 9))  `ShouldBe`  (Field 9 :*: Field 7)
            ]
testAlign = Valid

--

type TestField0 =  "test" :=> Field 19
testFieldPosition0
   :: Expect (GetFieldPositionUnsafe TestField0 "test" `ShouldBe` '(0,18))
testFieldPosition0 = Valid

type TestField1 =
      Field 1
  :*: "foo" :=> Flag
  :*: Field 8
  :*: "bar" :=> Field 5
  :*: "baz" :=> Field 9

testFieldPosition1Foo
   :: Expect (GetFieldPositionUnsafe TestField1 "foo" `ShouldBe` '(1,1))
testFieldPosition1Foo = Valid

testFieldPosition1Bar
   :: Expect (GetFieldPositionUnsafe TestField1 "bar" `ShouldBe` '(10,14))
testFieldPosition1Bar = Valid

testFieldPosition1Baz
   :: Expect (Try (GetFieldPosition TestField1 "baz") `ShouldBe` '(15,23))
testFieldPosition1Baz = Valid

-- (Proxy :: Proxy ("bar" :/ "foo"))
type TestFieldNested =
     Field 13
     :*: "bar" :=> (              Field 7
                    :*:           Flag
                    :*: "foo" :=> Field 16
                    :*:           Flag
                    :*:           Flag)

testFieldPositionTestFieldNested
   :: Expect
       (Try
         (GetFieldPosition TestFieldNested ("bar" :/ "foo"))
             `ShouldBe`
                        '(13 + 7 + 1, (16 - 1) + (13 + 7 + 1)))
testFieldPositionTestFieldNested = Valid

--

testFieldPositionToList
   :: Expect
       (FieldPostitionToList
         '(15,23)
       `ShouldBe`
       '[15,16,17,18,19,20,21,22,23])
testFieldPositionToList = Valid
