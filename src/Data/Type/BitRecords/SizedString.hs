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
import Data.Kind (type Type)

-- * String Fields


-- | A type level symbol paied with a type level length, that determines how
-- many characters of the symbol may be used. The first parameter defines the
-- length field.
data SizedString ::
     Symbol ->
     Nat ->
     Nat ->
     Nat ->  IsA BitRecordField

-- | The value level demoted type. Used internally.
data SizedStringDemRep :: Symbol -> Nat -> Type

type instance
     SizeFieldValue (SizedString str charCount byteCount bitCount) =
     charCount

type instance
     Eval (SizedString str charCount byteCount bitCount) =
     'MkField SizedString bitCount := SizedStringDemRep str byteCount

type instance
     ToPretty SizedString = PutStr "SizedString"

type instance
     ToPretty (SizedStringDemRep str byteCount) =
       PrettySurrounded (PutStr "<<") (PutStr ">>")
        (PutStr "utf-8[" <++> PutNat byteCount <++> PutStr " Bytes]:" <+> PutStr str)

-- | Create a 'SizedString' from a utf-8 string
utf8 :: TH.QuasiQuoter
utf8 = TH.QuasiQuoter undefined undefined mkSizedStr undefined
  where mkSizedStr :: String -> TH.Q TH.Type
        mkSizedStr str =
          do let strT = TH.LitT (TH.StrTyLit str)
                 charCount = fromIntegral $ length str
                 charCountT = TH.LitT (TH.NumTyLit charCount)
                 byteCount =
                   fromIntegral (B.length (E.encodeUtf8 (T.pack str)))
                 byteCountT = TH.LitT (TH.NumTyLit byteCount)
                 bitCountT = TH.LitT (TH.NumTyLit (8 * byteCount))
             return $
               TH.PromotedT ''SizedString `TH.AppT` strT `TH.AppT` charCountT `TH.AppT` byteCountT `TH.AppT` bitCountT

instance forall str byteCount f a . KnownSymbol str =>
  BitStringBuilderHoley (Proxy ('AssignF (SizedStringDemRep str byteCount) f)) a where
  bitStringBuilderHoley _ =
    immediate (appendStrictByteString (E.encodeUtf8 (T.pack (symbolVal (Proxy :: Proxy str)))))
