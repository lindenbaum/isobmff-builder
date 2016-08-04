{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A module of Orphan instances for e.g. for e.g. 'Default'.
module Data.ByteString.IsoBaseFileFormat.Util.Orphanage where

import qualified Data.ByteString as B
import           Data.Default
import           Data.Tagged
import qualified Data.Text       as T


instance Default B.ByteString where
  def = mempty

instance Default T.Text where
  def = ""

instance Default c => Default (Tagged s c) where
  def = Tagged def
