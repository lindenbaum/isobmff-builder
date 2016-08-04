-- | Brand/Box-validation
module Data.ByteString.IsoBaseFileFormat.MediaFile
        where

import Data.ByteString.IsoBaseFileFormat.Box
import Data.ByteString.IsoBaseFileFormat.Util.TypeLayout
import Data.ByteString.IsoBaseFileFormat.ReExports

-- TODO move this to general module merge with Box.hs
-- | A class that describes (on the type level) how a box can be nested into
-- other boxes (see 'Boxes).
class IsMediaFileFormat brand where
  -- | The layout that an IsBoxContent instance has to have, before 'packMedia' accepts it
  type BoxLayout brand
  mediaBuilder
    :: forall t proxy . (IsBoxContent t, IsRuleConform t (BoxLayout brand) ~ 'True)
    => proxy brand -> t -> Builder
  mediaBuilder _ t = boxBuilder t
