-- | Boxfield indicating an ISO 639-2 alpha-3 language code.
module Data.ByteString.IsoBaseFileFormat.Boxes.Language (Language, mkLanguage) where

import           Data.ByteString.IsoBaseFileFormat.Box

-- | A Boxfield contains an ISO 639-2-T alpha-3 language code,  which is encoded
-- as a single bit followed by three 5 bit words, each representing the
-- lower-case character from the language code subtracted by 0x60.
-- If available, the code for /terminoligy/ purposes should be used (T-code).
newtype Language = Language Word16 -- TODO use bitrecords

-- | Create a 'Language', throw a runtime error when bad characters were given.
-- The characters must be in @a - z@ (lower-case!), there must be exactly three
-- characters.
mkLanguage :: String -> Language
mkLanguage  [c0,c1,c2] |
   c0 > toEnum 0x60 && c0 < toEnum 0x7b &&
   c1 > toEnum 0x60 && c1 < toEnum 0x7b &&
   c2 > toEnum 0x60 && c2 < toEnum 0x7b =
     let
         s :: Char -> Word16
         s c = fromIntegral (fromEnum c) - 0x60
         in Language (shiftL (s c0) 10 .|.  shiftL (s c1) 5 .|. s c2)

mkLanguage other =
  error $
    "Invalid ISO 639-2 language code: " ++ show other ++
    ". The code must consist of three lower-case letters between 'a' .. 'z'."

instance IsString Language where
  fromString = mkLanguage

instance Default Language where
  def = mkLanguage "deu"

instance IsBoxContent Language where
  boxSize _ = 2
  boxBuilder (Language c) = word16BE c
