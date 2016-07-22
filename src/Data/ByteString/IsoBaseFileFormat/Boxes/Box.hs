{-# LANGUAGE UndecidableInstances #-}

-- | Definition of the most basic element in an ISOBMFF file: a /box/.  See
-- Chapter 4 in the standard document.  A box is a container with a type, a
-- size, some data and some nested boxes. The standard defines - among other
-- characteristics - available box types and their semantics, the fields they
-- contain and how they are nested into each other.  This library tries to
-- capture some of these characteristics using modern Haskell type system
-- features, in order to provide compile time checks for (partial) standard
-- compliance.
module Data.ByteString.IsoBaseFileFormat.Boxes.Box
       (module Data.ByteString.IsoBaseFileFormat.Boxes.Box, module X)
       where

import Data.Bits as X
import Data.ByteString.Builder as X
import Data.Monoid as X
import Data.Proxy as X
import Data.Word as X
import GHC.TypeLits as X
import Data.String
import Data.Type.Equality
import Data.Type.List
import Data.Type.Bool
import qualified Data.ByteString as B

-- * Basic Types and classes

-- | A class that describes (on the type level) how a box can be nested into
-- other boxes (see 'Boxes').
class BoxRules (t :: k) where
  -- | List of boxes that this box can be nested into.
  type RestrictedTo t :: Maybe [k]
  type RestrictedTo t = 'Just '[]
  -- | If the box is also allowed 'top-level' i.e. in the file directly, not
  -- nested in an other box.
  type IsTopLevelBox t :: Bool
  type IsTopLevelBox t = 'True
  -- | Describes which nested boxes MUST be present in a box using 'boxes'.
  type RequiredNestedBoxes t :: [k]
  type RequiredNestedBoxes t = '[]
  -- | Describes how many times a box should be present in a container (-box).
  type GetCardinality t (c :: k) :: Cardinality
  type GetCardinality t any = 'ExactlyOnce

-- | Describes how many times a box should be present in a container.
data Cardinality = AtMostOnce | ExactlyOnce | OnceOrMore

-- | Convert type level box types to values
class BoxRules t => IsBoxType (t :: k) where
  toBoxType :: proxy t -> BoxType

instance (BoxRules t, KnownSymbol t) => IsBoxType t where
  toBoxType _ = StdType (fromString (symbolVal (Proxy :: Proxy t)))

-- | Types that go into a box. A box content is a piece of data that can be
-- reused in different instances of 'IsBox'. It has no 'BoxType' and hence
-- defines no box.
class IsBoxContent a  where
  boxSize :: a -> BoxSize
  boxBuilder :: a -> Builder

-- | An empty box content can by represented by @()@ (i.e. /unit/).
instance IsBoxContent () where
  boxSize _ = 0
  boxBuilder _ = mempty

-- | Trivial instance for 'ByteString'
instance IsBoxContent B.ByteString where
  boxSize = fromIntegral . B.length
  boxBuilder = byteString

-- | Box content composed of box contents @a@ and @b@.
data Extend a b =
  Extend a
         b

-- | An operator for 'Extend'.
type a :+ b = Extend a b

-- | An operator for 'Extend'.
(<+>) :: a -> b -> Extend a b
(<+>) = Extend

instance (IsBoxContent p,IsBoxContent c) => IsBoxContent (Extend p c) where
  boxSize (Extend p c) = boxSize p + boxSize c
  boxBuilder (Extend p c) = boxBuilder p <> boxBuilder c

-- * Boxes



-- | Base class for all (abstract/phantom/normal-) types that represent boxes
class (IsBoxContent (BoxContent t), BoxRules t) => IsBoxType' t where
  type BoxContent t
  type BoxContent t = ()
  toBoxType' :: proxy t -> BoxType

-- | A type that wraps the contents of a box and the box type.
data Box' b where
  Box' :: (IsBoxType' b, ValidBoxes b ts) => BoxContent b -> Boxes' ts -> Box' b

instance IsBoxContent (Box' cnt) where
  boxBuilder b@(Box' cnt nested) = sB <> tB <> sExtB <> tExtB <> cntB <> nestedB
    where s       = boxSize    b
          t       = toBoxType' b
          sB      = boxBuilder s
          sExtB   = boxBuilder (BoxSizeExtension s)
          tB      = boxBuilder t
          tExtB   = boxBuilder (BoxTypeExtension t)
          cntB    = boxBuilder cnt
          nestedB = boxBuilder nested

  boxSize b@(Box' cnt nested) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =   boxSize (BoxSize undefined)
                     + boxSize t
                     + boxSize cnt
                     + boxSize (BoxTypeExtension t)
                     + boxSize nested
          t        = toBoxType' b


-- | A heterogenous collection of child boxes for a parent box wiht type @cont@.
data Boxes' (boxTypes :: [k]) where
        Nested' :: Boxes' '[]
        (:.) :: Boxes' ts -> Box' t -> Boxes' (t ': ts)

infixl 2 :.

instance IsBoxContent (Boxes' bs) where
  boxSize Nested' = 0
  boxSize (bs :. b) = boxSize bs + boxSize b
  boxBuilder Nested' = mempty
  boxBuilder (bs :. b) = boxBuilder bs <> boxBuilder b

-- | A box that contains no nested boxes.
closedBox :: (IsBoxType' t, ValidBoxes t '[]) => BoxContent t -> Box' t
closedBox c = Box' c Nested'

-- | A box that contains no fields, but nested boxes.
containerBox :: (IsBoxType' t, ValidBoxes t ts, BoxContent t ~ ()) => Boxes' ts -> Box' t
containerBox = Box' ()

-- | A complete media file, consisting of top-level boxes.
mediaFile :: Boxes' ts -> Builder
mediaFile = boxBuilder





-- | Create a 'Box' from any 'IsBoxContent' with a type.
box :: forall t c.
       (IsBoxType t,IsBoxContent c)
    => c -> Box t
box cnt = Box (toBoxType (Proxy :: Proxy t)) cnt

-- | An /empty/ box. This is for boxes without any extra /fields/. Although it is
-- called /emptyBox/ it might still contain nested boxes.
emptyBox :: forall t . (IsBoxType t) => Box t
emptyBox = box ()

-- | A type that wraps the contents of a box and the box type.
data Box (b :: t) where
        Box :: (IsBoxType t,IsBoxContent c) => BoxType -> c -> Box t

instance IsBoxContent (Box t) where
  boxBuilder b@(Box t cnt) = sFix <> tFix <> sExt <> tExt <> boxBuilder cnt
    where s = boxSize b
          sFix = boxBuilder s
          sExt = boxBuilder (BoxSizeExtension s)
          tFix = boxBuilder t
          tExt = boxBuilder (BoxTypeExtension t)
  boxSize (Box t cnt) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =
            boxSize (BoxSize 2) + boxSize t + boxSize cnt +
            boxSize (BoxTypeExtension t)

-- * Box Size and Type

-- | The size of the box. If the size is limited to a (fixed) value, it can be
-- provided as a 'Word64' which will be represented as either a 32bit compact
-- size or as 64 bit /largesize/. If 'UnlimitedSize' is used, the box extends to
-- the end of the file.
data BoxSize
  = UnlimitedSize
  | BoxSize Word64
  deriving (Show,Eq)

instance IsBoxContent BoxSize where
  boxSize _ = BoxSize 4
  boxBuilder UnlimitedSize = word32BE 0
  boxBuilder (BoxSize n) =
    word32BE $
    if n < (4294967296 :: Word64)
       then fromIntegral n
       else 1

instance Num BoxSize where
  (+) UnlimitedSize _ = UnlimitedSize
  (+) _ UnlimitedSize = UnlimitedSize
  (+) (BoxSize l) (BoxSize r) = BoxSize (l + r)
  (-) UnlimitedSize _ = UnlimitedSize
  (-) _ UnlimitedSize = UnlimitedSize
  (-) (BoxSize l) (BoxSize r) = BoxSize (l - r)
  (*) UnlimitedSize _ = UnlimitedSize
  (*) _ UnlimitedSize = UnlimitedSize
  (*) (BoxSize l) (BoxSize r) = BoxSize (l * r)
  abs UnlimitedSize = UnlimitedSize
  abs (BoxSize n) = BoxSize (abs n)
  signum UnlimitedSize = UnlimitedSize
  signum (BoxSize n) = BoxSize (signum n)
  fromInteger n = BoxSize $ fromInteger n

-- | The 'BoxSize' can be > 2^32 in which case an 'BoxSizeExtension' must be
-- added after the type field.
data BoxSizeExtension =
  BoxSizeExtension BoxSize

instance IsBoxContent BoxSizeExtension where
  boxBuilder (BoxSizeExtension UnlimitedSize) = mempty
  boxBuilder (BoxSizeExtension (BoxSize n)) =
    if n < 4294967296
       then mempty
       else word64BE n
  boxSize (BoxSizeExtension UnlimitedSize) = 0
  boxSize (BoxSizeExtension (BoxSize n)) =
    BoxSize $
    if n < 4294967296
       then 0
       else 8

-- | A box has a /type/, this is the value level representation for the box type.
data BoxType
  =
    -- | `FourCc` can be used as @boxType@ in `Box`, standard four letter character
    -- code, e.g. @ftyp@
    StdType FourCc
  |
    -- | CustomBoxType defines custom @boxType@s in `Box`es.
    CustomBoxType String
  deriving (Show,Eq)

-- | A type containin a printable four letter character code.
newtype FourCc =
  FourCc (Char,Char,Char,Char)
  deriving (Show,Eq)

instance IsString FourCc where
  fromString str
    | length str == 4 =
      let [a,b,c,d] = str
      in FourCc (a,b,c,d)
    | otherwise =
      error ("cannot make a 'FourCc' of a String which isn't exactly 4 bytes long: " ++
             show str ++ " has a length of " ++ show (length str))

instance IsBoxContent FourCc where
  boxSize _ = 4
  boxBuilder (FourCc (a,b,c,d)) = putW a <> putW b <> putW c <> putW d
    where putW = word8 . fromIntegral . fromEnum

instance IsBoxContent BoxType where
  boxSize _ = boxSize (FourCc undefined)
  boxBuilder t =
    case t of
      StdType x -> boxBuilder x
      CustomBoxType _ -> boxBuilder (FourCc ('u','u','i','d'))

-- | When using custom types extra data must be written after the extra size
-- information. Since the box type and the optional custom box type are not
-- guaranteed to be consequtive, this type handles the /second/ part seperately.
data BoxTypeExtension =
  BoxTypeExtension BoxType

instance IsBoxContent BoxTypeExtension where
  boxSize (BoxTypeExtension (StdType _)) = 0
  boxSize (BoxTypeExtension (CustomBoxType _)) = 16 * 4
  boxBuilder (BoxTypeExtension (StdType _)) = mempty
  boxBuilder (BoxTypeExtension (CustomBoxType str)) =
    mconcat (map (word8 . fromIntegral . fromEnum)
                 (take (16 * 4) str) ++
             repeat (word8 0))

-- * Type-safe box composition

-- | A 'ParentBox' is a 'Box' but without it's children. It has no
-- 'IsBoxContent' instance, and it must be converted to a real 'Box' using
-- 'boxes'. This is to prevent creation of boxes with invalid children.
data ParentBox (b :: t) where
        ParentBox :: (IsBoxType t,IsBoxContent c) => BoxType -> c -> ParentBox t

-- | A parent box. Similar to 'box' but for 'ParentBox'es.
parentBox :: forall t c . (IsBoxType t, IsBoxContent c) => c -> ParentBox t
parentBox = toParentBox . box
  where toParentBox :: IsBoxType s => Box s -> ParentBox s
        toParentBox (Box t c) = ParentBox t c

-- | A 'ParentBox' without /fields/.
emptyParentBox :: forall t . (IsBoxType t) => ParentBox t
emptyParentBox = parentBox ()

-- | A box that containing nested boxes. The nesting is validated at compile
-- time using 'BoxRules'.
boxes :: (IsBoxType t,IsBoxContent (Boxes t ts))
      => ParentBox t -> Boxes t ts -> Box t
boxes (ParentBox t c) = Box t . Extend c

-- | An operator for starting a 'Boxes' from the parent box.
--
-- Example:
-- >  xxx :: Box "moov"
-- >  xxx = movieBox
-- >         ^- Nested (movieHeaderBox (MovieHeader ...))
-- >                   :- (trackBox
-- >                       ^- Nested (trackHeaderBox (TrackHeader ...))
-- >                                 :- trackReferenceBox (TrackReference ...)
-- >                                 :- trackGroupingIndication (TrackGroupingInd ...))
--
(^-) :: (IsBoxType t,IsBoxContent (Boxes t ts))
     => ParentBox t -> Boxes t ts -> Box t
parent ^- nested = boxes parent nested

infixr 1 ^-

-- | A heterogenous collection of child boxes for a parent box wiht type @cont@.
data Boxes (cont :: x) (boxTypes :: [x]) where
        Nested :: IsBoxType t => Box t -> Boxes c '[t]
        (:-) :: IsBoxType t => Boxes c ts -> Box t -> Boxes c (t ': ts)

infixl 2 :-

-- | To be nested into a box, 'Boxes' must be an instance of 'IsBoxContent'.
-- This instance concatenates all nested boxes.
instance (ValidBoxes t bs) => IsBoxContent (Boxes t bs) where
  boxSize bs = boxSize (UnverifiedBoxes bs)
  boxBuilder bs = boxBuilder (UnverifiedBoxes bs)

-- | An internal wrapper type around 'Boxes' for the 'IsBoxContent' instance.
-- Since the 'IsBoxContent' instance recursivly deconstructs a 'Boxes' the
-- constraints for the validity 'ValidBoxes' cannot be asserted. To circumvent
-- this the 'IsBoxContent' instance for 'Boxes' delegates to the instance of
-- 'UnverifiedBoxes', which has no 'ValidBoxes' constraint in the instance head.
newtype UnverifiedBoxes t ts = UnverifiedBoxes (Boxes t ts)

instance IsBoxContent (UnverifiedBoxes t bs) where
  boxSize (UnverifiedBoxes (Nested b)) = boxSize b
  boxSize (UnverifiedBoxes (bs :- b)) = boxSize (UnverifiedBoxes bs) + boxSize b
  boxBuilder (UnverifiedBoxes (Nested b)) = boxBuilder b
  boxBuilder (UnverifiedBoxes (bs :- b)) = boxBuilder (UnverifiedBoxes bs) <> boxBuilder b

-- * Type level consistency checks

-- | A type-level check that uses 'BoxRules' to check that the contained boxes
-- are standard conform.
type ValidBoxes t ts =
  ( AllAllowedIn t ts ~ 'True
  , HasAllRequiredBoxes t (RequiredNestedBoxes t) ts ~ 'True )

-- | A type function to check that all nested boxes are allowed in the
-- container.
type family AllAllowedIn (container :: k) (boxes :: [k]) :: Bool
     where
        AllAllowedIn c '[] = 'True
        AllAllowedIn c (t ': ts) =
          If (CheckAllowedIn c t (RestrictedTo t))
             (AllAllowedIn c ts)
             (TypeError (NotAllowedMsg c t))

type family CheckAllowedIn (c :: k) (t :: k) (a :: Maybe [k]) :: Bool where
  CheckAllowedIn c t 'Nothing   = 'True
  CheckAllowedIn c t ('Just rs) = Find c rs


-- | The custom (type-) error message for 'AllAllowedIn'.
type NotAllowedMsg c t =
  'Text "Boxes of type: "
  ':<>: 'ShowType c
  ':<>: 'Text " may not contain boxes of type "
  ':<>: 'ShowType t
  ':$$: 'Text "Valid containers for "
  ':<>: 'ShowType t
  ':<>: 'Text " boxes are: "
  ':$$: 'ShowType (RestrictedTo t)
  ':$$: 'ShowType t
  ':<>: If (IsTopLevelBox c)
          ('Text " boxes may appear top-level in a file.")
          ('Text " boxes must be nested.")


-- | Check that all required boxes have been nested.
type family HasAllRequiredBoxes (c :: k) (req :: [k]) (nested :: [k]) :: Bool
     where
       HasAllRequiredBoxes c '[] nested = 'True
       HasAllRequiredBoxes c (r ': restReq) nested =
         If (Find r nested)
            (HasAllRequiredBoxes c restReq nested)
            (TypeError (MissingRequired c r nested))

type IsSubSet base sub = Intersection base sub == sub

-- | The custom (type-) error message for 'HasAllRequiredBoxes'.
type MissingRequired c r nested =
  'Text "Boxes of type: "
  ':<>: 'ShowType c
  ':<>: 'Text " require these nested boxes: "
  ':<>: 'ShowType (RequiredNestedBoxes c)
  ':$$: 'Text "but only these box types were nested: "
  ':<>: 'ShowType nested
  ':$$: 'Text "e.g. this type is missing: "
  ':<>: 'ShowType r

-- | Check that all boxes may appear top-level.
type family CheckAllTopLevelOk (ts :: [k]) :: Bool where
   CheckAllTopLevelOk '[] = 'True
   CheckAllTopLevelOk (t ': rest) = CheckTopLevelOk t && CheckAllTopLevelOk rest

-- | Check that the box may appear top-level.
type family CheckTopLevelOk (t :: k) :: Bool where
   CheckTopLevelOk t = IsTopLevelBox t || TypeError (NotTopLevenError t)


-- | The custom (type-) error message indicating that a box may not appear
-- top-level.
type NotTopLevenError c =
        'Text "Boxes of type "
  ':<>: 'ShowType c
  ':<>: 'Text " MUST be nested inside boxes of these types: "
  ':$$: 'ShowType (RestrictedTo c)

-- * Full Boxes

-- | A `Box` with /version/ and /branding/ information
type FullBox t = Extend FullBoxHeader t

-- | Create a 'FullBox' from a 'FourCc' 'StdType' and the nested box content.
fullBox
 :: (IsBoxType t, IsBoxContent c)
 => BoxVersion -> BoxFlags 24 -> c -> Box t
fullBox ver fs cnt = box (Extend (FullBoxHeader ver fs) cnt)

-- | The additional header with /version/ and /branding/ information
data FullBoxHeader =
  FullBoxHeader BoxVersion
                (BoxFlags 24)

instance IsBoxContent FullBoxHeader where
  boxSize (FullBoxHeader _ f) = 1 + boxSize f
  boxBuilder (FullBoxHeader (BoxVersion v) f) = word8 v <> boxBuilder f

-- | The box version (in a 'FullBox') is a single byte
newtype BoxVersion =
  BoxVersion Word8

-- | In addition to a 'BoxVersion' there can be 24 bits for custom flags etc in
-- a 'FullBox'.
newtype BoxFlags bits =
  BoxFlags Integer
  deriving (Eq,Show,Num)

-- | Internal function that creates a bit mask with all bits in a 'BoxFlags' set
-- to 1.
boxFlagBitMask :: KnownNat bits
               => BoxFlags bits -> Integer
boxFlagBitMask px = 2 ^ natVal px - 1

-- | Internal function that masks-out all bits higher than 'bits'.
cropBits :: KnownNat bits
         => BoxFlags bits -> BoxFlags bits
cropBits f@(BoxFlags b) = BoxFlags (b .&. boxFlagBitMask f)

-- | Get the number of bytes required to store a number of bits.
instance KnownNat bits => IsBoxContent (BoxFlags bits) where
  boxSize f =
    let minBytes = fromInteger $ natVal f `div` 8
        modBytes = fromInteger $ natVal f `mod` 8
    in BoxSize $ minBytes + signum modBytes
  boxBuilder f@(BoxFlags b) =
    let bytes =
          let (BoxSize bytes') = boxSize f
          in fromIntegral bytes'
        wordSeq n
          | n <= bytes =
            word8 (fromIntegral (shiftR b ((bytes - n) * 8) .&. 255)) <>
            wordSeq (n + 1)
          | otherwise = mempty
    in wordSeq 1

instance KnownNat bits => Bits (BoxFlags bits) where
  (.&.) (BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ l .&. r
  (.|.) (BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ l .&. r
  xor (BoxFlags l) (BoxFlags r) = cropBits $ BoxFlags $ xor l r
  complement (BoxFlags x) = cropBits $ BoxFlags $ complement x
  shift (BoxFlags x) = cropBits . BoxFlags . shift x
  rotateL = error "TODO rotateL"
  rotateR = error "TODO rotateR"
  bitSize = fromInteger . natVal
  bitSizeMaybe = Just . fromInteger . natVal
  isSigned _ = False
  testBit f n =
    let (BoxFlags b) = cropBits f
    in testBit b n
  bit = cropBits . BoxFlags . bit
  popCount f =
    let (BoxFlags b) = cropBits f
    in popCount b
  zeroBits = BoxFlags 0
