{-# LANGUAGE UndecidableInstances #-}
module BitRecordsSpec (spec) where

import Data.Bits
import Data.ByteString.IsoBaseFileFormat.Util.BitRecords
import Data.Proxy
import Data.Word
import Data.Type.Equality
import Data.Type.Bool
import Data.Monoid
import Data.ByteString.Builder
import Control.Category
import GHC.TypeLits
import Data.Bits
import Test.Hspec
import Test.TypeSpecCrazy
import Data.Int
import Text.Printf
import Debug.Trace
import qualified Data.ByteString.Lazy as B
import Data.Kind hiding (type (*))
import Prelude hiding ((.), id)

spec :: Spec
spec = do
  describe "The Set of Type Functions" $
    it "is sound" $ do
      print (Valid :: Expect (GetRecordSize (Flag :>: Field 7) `Is` 8))
      print testBitHasFields
      print testBitHasNestedFields
      print testRem
      print testAlign
      print testFieldPosition0
      print testFieldPosition1Foo
      print testFieldPosition1Bar
      print testFieldPosition1Baz
      print testFieldPositionTestFieldNested
      print testFieldPositionToList
      print checkHoleyTest64
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
  describe "BufferedBitBuilder" $ do
     it "writes fields" $
        let rec = Proxy
            rec :: Proxy  HoleyTest64
            actualB :: Builder
            actualB = toBuilder $ runBitBuilder64LE (toBitBuilder rec) 1 2 3
            actual = B.unpack $ toLazyByteString actualB
            in actual `shouldBe` [3,0,0,0,0,0,0,6]


type HoleyTest64 =
  "bar" :=> Field 6       :<:
  "baz" :=> Field 2       :<:
            Field 48 := 0 :<:
  "foo" :=> Field 8

checkHoleyTest64
  :: Expect '[ ShouldBe 64        (GetRecordSize HoleyTest64)
             , ShouldBe '(58, 63) (GetFieldPositionUnsafe HoleyTest64 "bar")
             , ShouldBe '(56, 57) (GetFieldPositionUnsafe HoleyTest64 "baz")
             , ShouldBe '(0, 7)   (GetFieldPositionUnsafe HoleyTest64 "foo") ]
checkHoleyTest64 = Valid

--

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

--

testAlign
  :: Expect '[ (Align 'True 7 Flag)       `ShouldBe`  (Flag :>: Ignore (Field 6 := 0))
             , (Align 'True 1 Flag)       `ShouldBe`  Flag
             , (Align 'True 8 (Field 7))  `ShouldBe`  (Field 7 :>: Ignore (Field 1 := 0))
             , (Align 'True 8 (Field 8))  `ShouldBe`  Field 8
             , (Align 'True 8 (Field 9))  `ShouldBe`  (Field 9 :>: Ignore (Field 7 := 0))
            ]
testAlign = Valid

--

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

-- (Proxy :: Proxy ("bar" :/ "foo"))
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

--

testFieldPositionToList
   :: Expect
       (FieldPostitionToList
         '(15,23)
       `ShouldBe`
       '[15,16,17,18,19,20,21,22,23])
testFieldPositionToList = Valid

-- * HoleyBit

type BitOffset = Int

data Alignment = Align8 | Align16 | Align32 | Align64

type family GetAlignmentBits (a :: Alignment) :: Nat where
  GetAlignmentBits 'Align8 = 8
  GetAlignmentBits 'Align16 = 16
  GetAlignmentBits 'Align32 = 32
  GetAlignmentBits 'Align64 = 64

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

type family AlignedWord (alignemnt :: Alignment) (signed :: Bool) where
  AlignedWord 'Align64 'True  =  Int64
  AlignedWord 'Align64 'False = Word64
  AlignedWord 'Align32 'True  =  Int32
  AlignedWord 'Align32 'False = Word32
  AlignedWord 'Align16 'True  =  Int16
  AlignedWord 'Align16 'False = Word16
  AlignedWord 'Align8  'True  =   Int8
  AlignedWord 'Align8  'False =  Word8

data Endianness = BigEndian | LittleEndian

newtype BitBuffer (a :: Alignment) (endianness :: Endianness) =
  BitBuffer {unBBWord :: AlignedWord a 'False}

deriving instance Eq (AlignedWord a 'False) => Eq (BitBuffer a e)
deriving instance Ord (AlignedWord a 'False) => Ord (BitBuffer a e)
deriving instance Num (AlignedWord a 'False) => Num (BitBuffer a e)
deriving instance Bits (AlignedWord a 'False) => Bits (BitBuffer a e)
deriving instance FiniteBits (AlignedWord a 'False) => FiniteBits (BitBuffer a e)

class (Num buff, FiniteBits buff) => IsBitBuffer buff where
  type GetAlignment buff :: Alignment
  wordBuilder :: buff -> Builder

instance IsBitBuffer (BitBuffer 'Align64 'LittleEndian) where
  wordBuilder = word64LE . unBBWord
instance IsBitBuffer (BitBuffer 'Align64 'BigEndian)    where
  wordBuilder = word64BE . unBBWord
instance IsBitBuffer (BitBuffer 'Align32 'LittleEndian) where
  wordBuilder = word32LE . unBBWord
instance IsBitBuffer (BitBuffer 'Align32 'BigEndian)    where
  wordBuilder = word32BE . unBBWord
instance IsBitBuffer (BitBuffer 'Align16 'LittleEndian) where
  wordBuilder = word16LE . unBBWord
instance IsBitBuffer (BitBuffer 'Align16 'BigEndian)    where
  wordBuilder = word16BE . unBBWord
instance IsBitBuffer (BitBuffer 'Align8 'LittleEndian) where
  wordBuilder = word8    . unBBWord
instance IsBitBuffer (BitBuffer 'Align8 'BigEndian)    where
  wordBuilder = word8    . unBBWord

------------------

type BitBuffer64BE = BitBuffer 'Align64 'BigEndian
runBitBuilder64BE :: HFinalBitBuilder BitBuffer64BE c -> c
runBitBuilder64BE = runBitBuilder
type BitBuffer32BE = BitBuffer 'Align32 'BigEndian
runBitBuilder32BE :: HFinalBitBuilder BitBuffer32BE c -> c
runBitBuilder32BE = runBitBuilder
type BitBuffer16BE = BitBuffer 'Align16 'BigEndian
runBitBuilder16BE :: HFinalBitBuilder BitBuffer16BE c -> c
runBitBuilder16BE = runBitBuilder
type BitBuffer64LE = BitBuffer 'Align64 'LittleEndian
runBitBuilder64LE :: HFinalBitBuilder BitBuffer64LE c -> c
runBitBuilder64LE = runBitBuilder
type BitBuffer32LE = BitBuffer 'Align32 'LittleEndian
runBitBuilder32LE :: HFinalBitBuilder BitBuffer32LE c -> c
runBitBuilder32LE = runBitBuilder
type BitBuffer16LE = BitBuffer 'Align16 'LittleEndian
runBitBuilder16LE :: HFinalBitBuilder BitBuffer16LE c -> c
runBitBuilder16LE = runBitBuilder
type BitBuffer8 = BitBuffer 'Align8 'LittleEndian
runBitBuilder8 :: HFinalBitBuilder BitBuffer8 c -> c
runBitBuilder8 = runBitBuilder

runBitBuilder :: IsBitBuffer buff => HFinalBitBuilder buff c -> c
runBitBuilder = runHoley

bindBitBuilder
  :: IsBitBuffer buff
  => HBuilder b c
  -> HBitBuilder buff a b
  -> HBuilder a c
bindBitBuilder !f !g =
  f `bind` (\b -> hoistIn (appBitBuilder b) g)

type HBuilder r a = Holey Builder r a
type HFinalBuilder a = HBuilder Builder a

type HBitBuilder buff r a = Holey (BitBuilder buff) r a
type HFinalBitBuilder buff a = HBitBuilder buff (BitBuilder buff) a


-- TODO BitBuilder add type level unflushed
toBuilder :: Num buff => BitBuilder buff -> Builder
toBuilder = appBitBuilder mempty

appBitBuilder :: Num buff => Builder -> BitBuilder buff -> Builder
appBitBuilder !b (BitBuilder !f) =
  bbStateBuilder (appEndo f (initialState b))

startBitBuilder :: Num buff => Builder -> BitBuilder buff
startBitBuilder b = modifyBitBuilder (const initialBBState b)

newtype BitBuilder buff = BitBuilder (Endo (BBState buff))
  deriving Monoid

modifyBitBuilder :: (BBState buff -> BBState buff) -> BitBuilder buff
modifyBitBuilder = BitBuilder . Endo

data BBState buff =
  BBState {  bbStateBuilder    :: !Builder
          , _bbStatePart       :: !buff
          , _bbStatePartOffset :: !BitOffset}

initialBBState :: Num buff => Builder -> BBState buff
initialBBState b = BBState b 0 0

-- | write bits starting from the most significant bit to the least
writeBits
      :: ( IsBitBuffer buff )
      => Int -> buff -> BitBuilder buff
writeBits !pLen !pBits = modifyBitBuilder $ go pLen pBits
  where
    go !0 _ !st = st
    go !len !bits (BBState !builder !part !partOffset) =
      let
          space = finiteBitSize part - partOffset
          writeLen = min len space
          readMask = 2 ^ writeLen - 1
          maskedBits = bits .&. readMask
          part' = part .|. (maskedBits `unsafeShiftL` partOffset)
      in
           if space > len
            then
              BBState builder part' (partOffset + len)
            else
              let restLen = len - writeLen
                  restBits = bits `unsafeShiftL` writeLen
                  nextBuilder = builder <> wordBuilder part'
              in go restLen restBits (BBState nextBuilder 0 0)

-------------------------
class HasBitBuilder field builderFun where
  type Res field builderFun
  type Res field builderFun = builderFun
  toBitBuilder
    :: IsBitBuffer b
    => proxy field
    -> HBitBuilder b builderFun (Res field builderFun)

instance ( KnownNat (GetRecordSize f) )
  => HasBitBuilder (l :=> f) r where
    type Res (l :=> f) r = Integer -> r
    toBitBuilder _ =
        indirect (writeBits fieldLen . fromIntegral)
      where
        fieldLen = fromIntegral (natVal (Proxy :: Proxy (GetRecordSize f)))

instance forall v f r .
         ( KnownNat v
         , KnownNat (GetRecordSize f))
  => HasBitBuilder (f := v) r where
    type Res (f := v) r = r
    toBitBuilder _ =
        immediate (writeBits fieldLen fieldVal)
      where
        fieldLen = fromIntegral (natVal (Proxy :: Proxy (GetRecordSize f)))
        fieldVal = fromIntegral (natVal (Proxy :: Proxy v))

instance ( HasBitBuilder f b
         , HasBitBuilder g a
         , b ~ Res g a)
  => HasBitBuilder (f :>: g) a where
    type Res (f :>: g) a = Res f (Res g a)
    toBitBuilder _ =
      toBitBuilder (Proxy :: Proxy f) % toBitBuilder (Proxy :: Proxy g)

-------------------------------------------------------------

newtype Holey m r a = HM {runHM :: ((m -> r) -> a) }

instance Monoid m => Category (Holey m) where
  (.) (HM f) (HM g) = HM (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))
  id = HM ($ mempty)

hoistIn :: (m -> n) -> Holey m a b -> Holey n a b
hoistIn into (HM f) = HM (\k -> f (k . into))

hoistOut :: (s -> r) -> Holey m r a -> Holey m s a
hoistOut outof (HM f) = HM (\k -> f (outof . k))

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


comp, comp', (%) :: Monoid m => Holey m b c -> Holey m a b  -> Holey m a c
comp f g = f `bind` (\l ->  g `bind` (\r -> immediate (l <> r)))
comp' (HM f) (HM g) = HM (\k -> (f (\m1 -> g (\m2 -> k (m1 <> m2)))))
(%) = comp'

runHoley :: Holey m m a -> a
runHoley = ($ id) . runHM
