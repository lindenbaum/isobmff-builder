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
import Data.Kind (type Type)
import Data.Kind.Extra

-- * String Fields

-- | A type level symbol paied with a type level length, that determines how
-- many characters of the symbol may be used. The first parameter defines the
-- length field.


data SizedStringRep :: Symbol -> Nat -> Type

data SizedString
  :: SizedStringRep str bytes
  -> IsA (BitRecordField ('MkFieldCustom (SizedStringRep str bytes) bytes))

type instance
     SizeFieldValue (SizedStringRep str byteCount) = byteCount


type instance
     ToPretty (SizedStringRep str byteCount) =
       PrettySurrounded (PutStr "<<") (PutStr ">>")
        (PutStr "utf-8[" <++> PutNat byteCount <++> PutStr " Bytes]:" <+> PutStr str)

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

instance forall str byteCount f a . KnownSymbol str =>
  BitStringBuilderHoley (Proxy (AssignF (SizedStringRep str byteCount) f)) a where
  bitStringBuilderHoley _ =
    immediate (appendStrictByteString (E.encodeUtf8 (T.pack (symbolVal (Proxy :: Proxy str)))))
