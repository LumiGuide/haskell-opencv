{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.JSON ( ) where

import "aeson" Data.Aeson
import "aeson" Data.Aeson.Types ( Parser )
import "aeson" Data.Aeson.TH
import qualified "base64-bytestring" Data.ByteString.Base64 as B64 ( encode, decode )
import "lens" Control.Lens hiding ( (.=) )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "lumi-hackage-extended" Data.Aeson.Extended ( lumiJsonOptions, delPrefix )
import qualified "text" Data.Text.Encoding as TE ( encodeUtf8, decodeUtf8 )
import "text" Data.Text ( Text )
import qualified "text" Data.Text as T ( unpack )
import "this" OpenCV.Core

--------------------------------------------------------------------------------

#define IsoJSON(ISO, FROM, TO)                   \
instance ToJSON FROM where {                     \
    toJSON = toJSON . (view ISO :: FROM -> TO);  \
};                                               \
instance FromJSON FROM where {                   \
    parseJSON = fmap (view $ from ISO :: TO -> FROM) . parseJSON; \
}

--------------------------------------------------------------------------------

IsoJSON(isoPoint2iV2, Point2i, V2 Int   )
IsoJSON(isoPoint2fV2, Point2f, V2 Float )
IsoJSON(isoPoint2dV2, Point2d, V2 Double)
IsoJSON(isoPoint3iV3, Point3i, V3 Int   )
IsoJSON(isoPoint3fV3, Point3f, V3 Float )
IsoJSON(isoPoint3dV3, Point3d, V3 Double)
IsoJSON(isoSize2iV2 , Size2i , V2 Int   )
IsoJSON(isoSize2fV2 , Size2f , V2 Float )
IsoJSON(hmat        , Mat    , HMat     )

--------------------------------------------------------------------------------

instance ToJSON HElems where
    toJSON = \case
        HElems_8U       v -> f "8U"  v
        HElems_8S       v -> f "8S"  v
        HElems_16U      v -> f "16U" v
        HElems_16S      v -> f "16S" v
        HElems_32S      v -> f "32S" v
        HElems_32F      v -> f "32F" v
        HElems_64F      v -> f "64F" v
        HElems_USRTYPE1 v -> f "USR" $ fmap (TE.decodeUtf8 . B64.encode) v
      where
        f :: (ToJSON a) => Text -> a -> Value
        f typ v = object [ "type"  .= typ
                         , "elems" .= v
                         ]

instance FromJSON HElems where
    parseJSON = withObject "HElems" $ \obj -> do
                  typ <- obj .: "type"
                  let elems :: (FromJSON a) => Parser a
                      elems = obj .: "elems"
                  case typ of
                    "8U"  -> HElems_8U       <$> elems
                    "8S"  -> HElems_8S       <$> elems
                    "16U" -> HElems_16U      <$> elems
                    "16S" -> HElems_16S      <$> elems
                    "32S" -> HElems_32S      <$> elems
                    "32F" -> HElems_32F      <$> elems
                    "64F" -> HElems_64F      <$> elems
                    "USR" -> HElems_USRTYPE1 <$> (mapM (either fail pure . B64.decode . TE.encodeUtf8) =<< elems)
                    _ -> fail $ "Unknown Helems type " <> T.unpack typ

instance ToJSON Rect where
    toJSON rect = object [ "pos"  .= rectTopLeft rect
                         , "size" .= rectSize    rect
                         ]

instance FromJSON Rect where
    parseJSON = withObject "Rect" $ \obj ->
                  mkRect <$> obj .: "pos" <*> obj .: "size"

--------------------------------------------------------------------------------

deriveJSON (lumiJsonOptions $ delPrefix "hm") ''HMat
deriveJSON (lumiJsonOptions id) ''V2
deriveJSON (lumiJsonOptions id) ''V3
deriveJSON (lumiJsonOptions id) ''V4
