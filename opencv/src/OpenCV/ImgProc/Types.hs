module OpenCV.ImgProc.Types
    ( InterpolationMethod(..)
    , BorderMode(..)
    ) where

import "this" OpenCV.Core.Types ( Scalar )

--------------------------------------------------------------------------------

data InterpolationMethod
   = InterNearest -- ^ Nearest neighbor interpolation.
   | InterLinear -- ^ Bilinear interpolation.
   | InterCubic -- ^ Bicubic interpolation.
   | InterArea
     -- ^ Resampling using pixel area relation. It may be a preferred method for
     -- image decimation, as it gives moire'-free results. But when the image is
     -- zoomed, it is similar to the 'InterNearest' method.
   | InterLanczos4 -- ^ Lanczos interpolation over 8x8 neighborhood
     deriving Show

-- TODO (RvD): Show instance
-- Needs a Show instance for Scalar
data BorderMode
   = BorderConstant Scalar -- ^ 1D example: @iiiiii|abcdefgh|iiiiiii@  with some specified @i@
   | BorderReplicate   -- ^ 1D example: @aaaaaa|abcdefgh|hhhhhhh@
   | BorderReflect     -- ^ 1D example: @fedcba|abcdefgh|hgfedcb@
   | BorderWrap        -- ^ 1D example: @cdefgh|abcdefgh|abcdefg@
   | BorderReflect101  -- ^ 1D example: @gfedcb|abcdefgh|gfedcba@
   | BorderTransparent -- ^ 1D example: @uvwxyz|absdefgh|ijklmno@
   | BorderIsolated    -- ^ do not look outside of ROI
