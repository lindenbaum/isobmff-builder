{-# LANGUAGE UndecidableInstances #-}

module Data.Type.BitRecords.SizedString
  (SizedString()
  ,utf8)
  where

import Data.Type.BitRecords.Core
import Data.Type.BitRecords.Sized
import Data.Type.BitRecords.Builder.Holey
import Data.Type.BitRecords.Builder.LazyByteStringBuilder
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH
import GHC.TypeLits
import Data.Type.Pretty
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Proxy

import Data.Kind.Extra

-- * String Fields

-- | A type level symbol paired with a type level length, that determines how
-- many characters of the symbol may be used. The first parameter defines the
-- length field.
type SizedString str bytes =
  MkField ('MkFieldCustom :: BitField ASizedString ASizedString (8 * bytes)) := 'MkASizedString str bytes

data ASizedString where
  MkASizedString :: Symbol -> Nat -> ASizedString

type instance
     SizeFieldValue ('MkASizedString str byteCount) = byteCount

type instance
     ToPretty ASizedString = PutStr "utf-8"

type instance PrettyCustomFieldValue ASizedString ASizedString s sr =
     ToPretty sr

type instance
     ToPretty ('MkASizedString str byteCount) =
       PrettySurrounded (PutStr "<<") (PutStr ">>") (PutStr str)<+> PutStr "[" <++> PutNat byteCount <++> PutStr " Bytes]"

-- | Create a 'SizedString' from a utf-8 string
utf8 :: TH.QuasiQuoter
utf8 = TH.QuasiQuoter undefined undefined mkSizedStr undefined
  where mkSizedStr :: String -> TH.Q TH.Type
        mkSizedStr str =
          do let strT = TH.LitT (TH.StrTyLit str)
                 byteCount =
                   fromIntegral (B.length (E.encodeUtf8 (T.pack str)))
                 byteCountT = TH.LitT (TH.NumTyLit byteCount)
             return $
               TH.PromotedT ''SizedString `TH.AppT` strT `TH.AppT` byteCountT

instance
  forall (size :: Nat) (str :: Symbol) (bytes :: Nat) r (f :: IsA (BitRecordField ('MkFieldCustom :: BitField ASizedString ASizedString size))) .
     (KnownSymbol str) =>
  BitStringBuilderHoley (Proxy (AssignF ('MkASizedString str bytes) f)) r where
  bitStringBuilderHoley _ = immediate (appendStrictByteString (E.encodeUtf8 (T.pack (symbolVal (Proxy :: Proxy str)))))
