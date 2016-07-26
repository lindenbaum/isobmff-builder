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
import Data.Kind
import GHC.TypeLits as X
import Data.String
import Data.Singletons.Prelude.List (Length)
import Data.Default
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Singletons.Prelude.List ((:++))
import qualified Data.ByteString as B

-- * Box Type Classes
-- | Base class for all (abstract/phantom/normal-) types that represent boxes
class (IsBoxContent (BoxContent t)) => IsBoxType t  where
  type BoxContent t
  type BoxContent t = ()
  toBoxType :: proxy t -> BoxContent t -> BoxType

-- | Types that go into a box. A box content is a piece of data that can be
-- reused in different instances of 'IsBox'. It has no 'BoxType' and hence
-- defines no box.
class IsBoxContent a  where
  boxSize :: a -> BoxSize
  boxBuilder :: a -> Builder

-- * Data types

-- | A type that wraps the contents of a box and the box type.
data Box b where
        Box ::
            (IsBoxType b) =>
            BoxContent b -> Box b

instance IsBoxContent (Box cnt) where
  boxBuilder b@(Box cnt) = sB <> tB <> sExtB <> tExtB <> cntB
    where s = boxSize b
          t = toBoxType b cnt
          sB = boxBuilder s
          sExtB = boxBuilder (BoxSizeExtension s)
          tB = boxBuilder t
          tExtB = boxBuilder (BoxTypeExtension t)
          cntB = boxBuilder cnt
  boxSize b@(Box cnt) = sPayload + boxSize (BoxSizeExtension sPayload)
    where sPayload =
            boxSize (BoxSize undefined) + boxSize t + boxSize cnt +
            boxSize (BoxTypeExtension t)
          t = toBoxType b cnt

-- | Compose 'BoxContent' and 'Boxes' under the Constraint that they are
-- composable.
data ContainerBox b (bs :: [Type])

instance (IsBoxType b, IsBoxContent (Boxes bs)) => IsBoxType (ContainerBox b bs) where
  type BoxContent (ContainerBox b bs) = BoxContent b :+ Boxes bs
  toBoxType _ (bCnt :+ _) = toBoxType (Proxy :: Proxy b) bCnt

-- | Container box wich includes a length field
data SizedContainerBox b (bs :: [Type]) where
  SizedContainerBox
    :: (KnownNat (Length bs), IsBoxType b)
    => (Integer -> BoxContent b)
    -> Boxes bs
    -> SizedContainerBox b bs

instance (IsBoxType (ContainerBox b bs)) => IsBoxType (SizedContainerBox b bs) where
    type BoxContent (SizedContainerBox b bs) = SizedContainerBox b bs
    toBoxType _ (SizedContainerBox f bs) =
      toBoxType (Proxy :: Proxy (ContainerBox b bs))
                (f (typeListLength bs) :+ bs)

instance (IsBoxType (ContainerBox b bs)) => IsBoxContent (SizedContainerBox b bs) where
    boxSize (SizedContainerBox f bs) = boxSize (f (typeListLength bs) :+ bs)
    boxBuilder (SizedContainerBox f bs) = boxBuilder (f (typeListLength bs) :+ bs)

-- | A heterogenous collection of boxes.
data Boxes (boxTypes :: [Type]) where
    NoBoxes :: Boxes '[]
    (:.) :: Box l -> Boxes r -> Boxes (l ': r)
    -- | Create a 'Boxes' collection from two 'Box'es
    (:|) :: Box l -> Box r -> Boxes '[l, r]
    (:<>) :: Boxes l -> Boxes r -> Boxes (l :++ r)

infixr 1 :<>
infixr 2 :.
infixr 2 :|
infixr 3 $:

-- | Apply a function to a 'Boxes' collection containing only a single 'Box'.
($:) :: (Boxes '[l] -> r) -> Box l -> r
($:) f = f . singletonBox

-- | Create a 'Boxes' collection with a single 'Box'.
singletonBox :: Box l -> Boxes '[l]
singletonBox b = b :. NoBoxes

-- | Get the elements in a type level array
typeListLength :: forall a proxy (ts :: [k]) . (KnownNat (Length ts), Num a)
               => proxy ts -> a
typeListLength _ = fromIntegral (natVal (Proxy :: Proxy (Length ts)))

instance IsBoxContent (Boxes bs) where
  boxSize NoBoxes = 0
  boxSize (l :. r) = boxSize l + boxSize r
  boxSize (l :| r) = boxSize l + boxSize r
  boxSize (l :<> r) = boxSize l + boxSize r
  boxBuilder NoBoxes = mempty
  boxBuilder (l :. r) = boxBuilder l <> boxBuilder r
  boxBuilder (l :| r) = boxBuilder l <> boxBuilder r
  boxBuilder (l :<> r) = boxBuilder l <> boxBuilder r

-- | A box that contains no fields, but nested boxes.
containerBox :: (IsBoxType t, BoxContent t ~ ())
             => Boxes ts -> Box (ContainerBox t ts)
containerBox bs = Box (() :+ bs)

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

-- | This 'Text' instance writes a null terminated UTF-8 string.
instance IsBoxContent T.Text where
  boxSize = (1+) . fromIntegral . T.length
  boxBuilder txt = boxBuilder (T.encodeUtf8 txtNoNulls) <> word8 0
    where txtNoNulls = T.map (\c -> if c == '\0' then ' ' else c) txt

-- * Box concatenation

-- | Box content composition
data a :+ b = a :+ b

infixr 3 :+

instance (IsBoxContent p,IsBoxContent c) => IsBoxContent (p :+ c) where
  boxSize (p :+ c) = boxSize p + boxSize c
  boxBuilder (p :+ c) = boxBuilder p <> boxBuilder c

instance (Default a, Default b) => Default (a :+ b) where
  def = def :+ def
