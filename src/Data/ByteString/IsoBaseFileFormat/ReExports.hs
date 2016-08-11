{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A module of re-exports and orphan instances
module Data.ByteString.IsoBaseFileFormat.ReExports (module X) where


import           Data.Bits                                         as X
import qualified Data.ByteString                                   as B
import           Data.ByteString.Builder                           as X
import           Data.Type.BitRecords                              as X
import           Data.ByteString.IsoBaseFileFormat.Util.TypeLayout as X
import           Data.Default                                      as X
import           Data.Foldable                                     as X (fold)
import           Data.Int                                          as X
import           Data.Kind                                         as X (Constraint,
                                                                         Type)
import           Data.Maybe                                        as X
import           Data.Monoid                                       as X
import           Data.Proxy                                        as X
import           Data.String                                       as X
import           Data.Tagged                                       as X
import qualified Data.Text                                         as T
import           Data.Type.Bool                                    as X
import           Data.Type.Equality                                as X
import           Data.Word                                         as X
import           GHC.TypeLits                                      as X
import           Test.TypeSpecCrazy                                as X hiding
                                                                         (type Not)
import           Text.Printf                                       as X
import Debug.Trace as X

instance Default B.ByteString where
  def = mempty

instance Default T.Text where
  def = ""

instance Default c => Default (Tagged s c) where
  def = Tagged def
