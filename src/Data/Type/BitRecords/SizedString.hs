{-# LANGUAGE UndecidableInstances #-}

module Data.Type.BitRecords.SizedString
  (SizedString()
  ,utf8)
  where

import Data.Type.BitRecords.Core
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

-- * String Fields
-- | A type level symbol paied with a type level length, that determines how
-- many characters of the symbol may be used. The first parameter defines the
-- length field.
data SizedString =
  SizedString Symbol
              Nat
              Nat
              Nat

type instance
     SizeFieldValue ('SizedString str charCount byteCount bitCount) =
     charCount

type instance
     GetFieldSize ('SizedString str charCount byteCount bitCount) =
     bitCount

type instance
     PrettyRecord ('SizedString str charCount byteCount bitCount) =
       PrettySurrounded (PutStr "<<") (PutStr ">>")
        (PutStr "utf-8[" <++> PutNat charCount <++> PutStr "]:" <+> PutStr str)

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
               TH.PromotedT 'SizedString `TH.AppT` strT `TH.AppT`
               charCountT `TH.AppT` byteCountT `TH.AppT` bitCountT

instance forall str charCount byteCount bitCount r . KnownSymbol str
  => BitStringBuilderHoley (Proxy ('SizedString str charCount byteCount bitCount)) r where
  bitStringBuilderHoley _ =
    immediate (appendStrictByteString (E.encodeUtf8 (T.pack (symbolVal (Proxy :: Proxy str)))))
