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

testAlign
  :: Expect '[ (Align 'True 7 Flag)       `ShouldBe`  (Flag :>: Ignore (Field 6 := 0))
             , (Align 'True 1 Flag)       `ShouldBe`  Flag
             , (Align 'True 8 (Field 7))  `ShouldBe`  (Field 7 :>: Ignore (Field 1 := 0))
             , (Align 'True 8 (Field 8))  `ShouldBe`  Field 8
             , (Align 'True 8 (Field 9))  `ShouldBe`  (Field 9 :>: Ignore (Field 7 := 0))
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
    describe "formatBits align16" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $ formatBits littleEndian align16 rec
                        (Tagged 1 :: Tagged "bar" Integer)
                        (Tagged 2 :: Tagged "baz" Integer)
                        (Tagged 4 :: Tagged "foo" Integer)

            actual = B.unpack $ toLazyByteString actualB
            in actual `shouldBe` [1,0,2,0,0,0,0,3]
    describe "formatBits align8" $ do
      it "writes fields" $
        let rec = Proxy
            rec :: Proxy TestRecUnAligned
            actualB :: Builder
            actualB = toFlushedBuilder $ formatBits littleEndian align8 rec
                        (Tagged 1 :: Tagged "bar" Integer)
                        (Tagged 2 :: Tagged "baz" Integer)
                        (Tagged 4 :: Tagged "foo" Integer)

            actual = B.unpack $ toLazyByteString actualB
            in actual `shouldBe` [1,0,2,0,0,0,0,3]

-- * Alignment

-- | Alignments for optimized writing of bits into bytestrings.
data Alignment = Align8 | Align16 | Align32 | Align64

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

-- | Types which contain a finite amount of bits, which can be set from a value
-- and an offset. Bits can be written to the value.
class DirectedBits a where
  -- | Copy bits starting at a specific offset from one @a@ the the other.
  copyBits
    :: Int  -- ^ @length@ of the value to write in number of bits.
    -> a -- ^ The value to write (in the lower @length@ bits).
    -> Int  -- ^ The start offset in the output value
    -> a -- ^ The input to write to
    -> (a, Int, Int) -- ^ The output buffer, space left in buffer,

  -- | Type level calculation for the rest of the buffer
  type CopyBitsRestLength a (copyLen :: Nat) (offset :: Nat) :: Nat

-- | Set bits starting from the most significant bit to the least.
--   For example @writeBits m 1 <> writeBits n 2@ would result in:
-- @
--         MSB                                             LSB
--    Bit: |k  ..  k-(m+1)|k-m  ..  k-(m+n+1)| k-(m+n)  ..  0|
--  Value: |0     ..     1|0        ..     10|  ...          |
--          ->             ->                 ->     (direction of writing)
-- @
instance ( FiniteBits (BitBuffer a e), HasBuilder (BitBuffer a e))
  => DirectedBits (BitBuffer a e) where
  copyBits !len !bits !offset !buff =
    let buffLen = builderAlignment buff
        spaceAvailable = buffLen - offset
        writeLen = min spaceAvailable len
        spaceLeft = spaceAvailable - writeLen
        writeOffset = spaceAvailable - writeLen
        readMask = 2 ^ writeLen - 1
        maskedBits = bits .&. readMask
        buff' = buff .|. (maskedBits `unsafeShiftL` writeOffset)
        in (buff', spaceLeft, writeLen)

  type CopyBitsRestLength (BitBuffer a e) len offset =
          GetRemainingUnaligned (len + offset) a

-- * Bit Buffering

-- | Words acting as aligned bit buffers, that can eventually be converted to a
-- 'Builder'.
class ( Num a, Show a, KnownNat (GetAlignmentBits (GetAlignment a)))
    => HasBuilder a where
  type GetAlignment a :: Alignment
  wordBuilder :: a -> Builder

-- | Return the static size of a 'HasBuilder'.
builderAlignmentProxy :: forall b proxy . (HasBuilder b) => proxy b -> Int
builderAlignmentProxy _ =
  fromIntegral $ natVal $ (Proxy :: Proxy (GetAlignmentBits (GetAlignment b)))

-- | Return the static size of an 'HasBuilder'. The parameter is ignored!
builderAlignment :: forall b . (HasBuilder b) => b -> Int
builderAlignment _ =
  fromIntegral $ natVal $ (Proxy :: Proxy (GetAlignmentBits (GetAlignment b)))

-- | A wrapper around an integral type retreived from 'ToAlignedWord' like
-- 'Word32', 'Word64', etc, that acts as a buffer for efficient serialization of
-- bits to a 'Builder'.
newtype BitBuffer (a :: Alignment) (endianness :: Endianness) =
  BitBuffer {fromBitBufferMsbFirst :: ToAlignedWord a}

instance PrintfArg (ToAlignedWord a) => Show (BitBuffer a e) where
  show (BitBuffer x) = printf "%0.64b" x

deriving instance Eq (ToAlignedWord a) => Eq (BitBuffer a e)
deriving instance Ord (ToAlignedWord a) => Ord (BitBuffer a e)
deriving instance Num (ToAlignedWord a) => Num (BitBuffer a e)
deriving instance Bits (ToAlignedWord a) => Bits (BitBuffer a e)
deriving instance FiniteBits (ToAlignedWord a) => FiniteBits (BitBuffer a e)

instance HasBuilder (BitBuffer 'Align64 'LittleEndian) where
  type GetAlignment (BitBuffer 'Align64 'LittleEndian) = 'Align64
  wordBuilder = word64LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align64 'BigEndian)    where
  type GetAlignment (BitBuffer 'Align64 'BigEndian) = 'Align64
  wordBuilder = word64BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align32 'LittleEndian) where
  type GetAlignment (BitBuffer 'Align32 'LittleEndian) = 'Align32
  wordBuilder = word32LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align32 'BigEndian)    where
  type GetAlignment (BitBuffer 'Align32 'BigEndian) = 'Align32
  wordBuilder = word32BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align16 'LittleEndian) where
  type GetAlignment (BitBuffer 'Align16 'LittleEndian) = 'Align16
  wordBuilder = word16LE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align16 'BigEndian)    where
  type GetAlignment (BitBuffer 'Align16 'BigEndian) = 'Align16
  wordBuilder = word16BE . fromBitBufferMsbFirst
instance HasBuilder (BitBuffer 'Align8 e) where
  type GetAlignment (BitBuffer 'Align8 e) = 'Align8
  wordBuilder = word8    . fromBitBufferMsbFirst

------------------

formatBits
  :: forall proxy0 proxy1 proxy2 rec endianness buff alignment res off recSize
   . ( buff ~ BitBuffer alignment endianness
     , FiniteBits (ToAlignedWord alignment)
     , HasBuilder buff
     , recSize ~ GetRecordSize rec
     , KnownNat recSize
     , off ~ GetRemainingUnaligned recSize alignment
     , KnownNat off
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

flushBuilder :: forall buff off . (KnownNat off, Num buff, HasBuilder buff)
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
      :: ( KnownNat len, HasBuilder buff, Bits buff, DirectedBits buff
         , KnownNat fromOffset
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
    go 0 _ !bldr !part _ = BBState bldr part
    go !len !bits !builder !part !partOffset =
      let
        (part', spaceLeft, writeLen) = copyBits len bits partOffset part
      in
           if spaceLeft > 0
            then
              trace "spaceLeft: " $ traceShow spaceLeft $
                BBState builder part
            else
              let restLen = len - writeLen
                  restBits = bits `unsafeShiftL` writeLen
                  nextBuilder = builder <> wordBuilder part'
              in trace "restLen: " $ traceShow restLen $
                  go restLen restBits nextBuilder 0 0

-------------------------

class HasFormatter m f r where
  type FmtArg m f r
  type FmtArg m f r = r
  toFormatter :: proxy f -> Holey m r (FmtArg m f r)

instance ( KnownNat oF, KnownNat oT, HasBuilder b
         , DirectedBits b, Bits b, KnownNat (GetRecordSize f)
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
          , DirectedBits b
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
         , DirectedBits bb
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
