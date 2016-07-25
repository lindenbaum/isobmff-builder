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

import Data.ByteString.IsoBaseFileFormat.Boxes.Brand as X
import Data.Bits as X
import Data.ByteString.Builder as X
import Data.Monoid as X
import Data.Proxy as X
import Data.Word as X
import Data.Kind
import GHC.TypeLits as X
import Data.String
import Data.Singletons.Prelude.List ((:++))
import qualified Data.ByteString as B

-- * Box Type Classes
-- | Base class for all (abstract/phantom/normal-) types that represent boxes
class (IsBoxContent (BoxContent t)) => IsBoxType' t  where
  type BoxContent t
  type BoxContent t = ()
  toBoxType' :: proxy t -> BoxType

-- | Types that go into a box. A box content is a piece of data that can be
-- reused in different instances of 'IsBox'. It has no 'BoxType' and hence
-- defines no box.
class IsBoxContent a  where
  boxSize :: a -> BoxSize
  boxBuilder :: a -> Builder

-- * Data types
-- | A type that wraps the contents of a box and the box type.
data Box brand b where
        Box ::
            (IsBoxType' b, IsBrandConform brand ('Just b) ts) =>
            BoxContent b -> Boxes brand ts -> Box brand b

instance IsBoxContent (Box brand cnt) where
  boxBuilder b@(Box cnt nested) = sB <> tB <> sExtB <> tExtB <> cntB <> nestedB
    where s = boxSize b
          t = toBoxType' b
          sB = boxBuilder s
          sExtB = boxBuilder (BoxSizeExtension s)
          tB = boxBuilder t
          tExtB = boxBuilder (BoxTypeExtension t)
          cntB = boxBuilder cnt
          nestedB = boxBuilder nested
  boxSize b@(Box cnt nested) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =
            boxSize (BoxSize undefined) + boxSize t + boxSize cnt +
            boxSize (BoxTypeExtension t) +
            boxSize nested
          t = toBoxType' b

-- | A heterogenous collection of boxes.
data Boxes brand (boxTypes :: [Type]) where
        NoBoxes :: Boxes brand '[]
        (:.) :: Box brand l -> Boxes brand r -> Boxes brand (l ': r)
        (:<>) :: Boxes brand l -> Boxes brand r -> Boxes brand (l :++ r)

infixr 2 :.

-- | Create a 'Boxes' collection with a single 'Box'.
singletonBox :: Box brand l -> Boxes brand '[l]
singletonBox b = b :. NoBoxes

-- | Create a 'Boxes' collection from two 'Box'es
(.:.) :: Box brand l -> Box brand r -> Boxes brand '[l, r]
(.:.) l r = l :. r :. NoBoxes

infixr 0 .:.

-- | Apply a function to a 'Boxes' collection containing only a single 'Box'.
($:) :: (Boxes brand '[l] -> r) -> Box brand l -> r
($:) f l = f $ l :. NoBoxes

infixr 2 $:

instance IsBoxContent (Boxes brand bs) where
  boxSize NoBoxes = 0
  boxSize (l :. r) = boxSize l + boxSize r
  boxSize (l :<> r) = boxSize l + boxSize r
  boxBuilder NoBoxes = mempty
  boxBuilder (l :. r) = boxBuilder l <> boxBuilder r
  boxBuilder (l :<> r) = boxBuilder l <> boxBuilder r

-- | A box that contains no nested boxes.
closedBox :: (IsBoxType' t, IsBrandConform brand ('Just t) '[])
          => BoxContent t -> Box brand t
closedBox c = Box c NoBoxes

-- | A box that contains no fields, but nested boxes.
containerBox :: (IsBoxType' t,IsBrandConform brand ('Just t) ts,BoxContent t ~ ())
             => Boxes brand ts -> Box brand t
containerBox = Box ()

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

-- * 'IsBoxContent' instances
-- | An empty box content can by represented by @()@ (i.e. /unit/).
instance IsBoxContent () where
  boxSize _ = 0
  boxBuilder _ = mempty

-- | Trivial instance for 'ByteString'
instance IsBoxContent B.ByteString where
  boxSize = fromIntegral . B.length
  boxBuilder = byteString-- -- | A list, a maybe, and every other 'Foldable' of contents is a content.
                         -- instance (Foldable f, IsBoxContent t) => IsBoxContent (f t) where
                         --   boxSize = foldr' (\e acc -> acc + boxSize e) 0
                         --   boxBuilder = foldMap boxBuilder
