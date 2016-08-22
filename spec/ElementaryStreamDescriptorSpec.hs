module ElementaryStreamDescriptorSpec ( spec ) where

import           Control.Category
import           Data.Bits
import           Data.ByteString.Builder
import           Data.ByteString.IsoBaseFileFormat.Box
import           Data.ByteString.IsoBaseFileFormat.ReExports
import           Data.ByteString.IsoBaseFileFormat.Util.BoxFields
import qualified Data.ByteString.Lazy                                 as B
import           Data.ByteString.Mp4.Boxes.ElementaryStreamDescriptor
import           Data.Type.BitRecords
import           Data.Word
import           Prelude                                              hiding
                                                                       ((.))
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = return ()
