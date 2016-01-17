{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenCV.JSON ( ) where

import "aeson" Data.Aeson
import "aeson" Data.Aeson.Types ( Parser )
import "aeson" Data.Aeson.TH
import qualified "base64-bytestring" Data.ByteString.Base64 as B64 ( encode, decode )
import "linear" Linear.V2 ( V2(..) )
import "linear" Linear.V3 ( V3(..) )
import "linear" Linear.V4 ( V4(..) )
import "lumi-hackage-extended" Lumi.Prelude
import "lumi-hackage-extended" Data.Aeson.Extended ( lumiJsonOptions, delPrefix )
import qualified "text" Data.Text.Encoding as TE ( encodeUtf8, decodeUtf8 )
import "text" Data.Text ( Text )
import qualified "text" Data.Text as T ( unpack )
import "this" OpenCV.Core.Types
import "this" OpenCV.Core.Types.Mat.HMat
import "this" OpenCV.TypeLevel

--------------------------------------------------------------------------------

#define IsoJSON(A, B, A_TO_B, B_TO_A)                \
instance ToJSON A where {                            \
    toJSON = toJSON . (A_TO_B :: A -> B);            \
};                                                   \
instance FromJSON A where {                          \
    parseJSON = fmap (B_TO_A :: B -> A) . parseJSON; \
}

--------------------------------------------------------------------------------

IsoJSON(Point2i, V2 Int32 , fromPoint2i, toPoint2i)
IsoJSON(Point2f, V2 Float , fromPoint2f, toPoint2f)
IsoJSON(Point2d, V2 Double, fromPoint2d, toPoint2d)
IsoJSON(Point3i, V3 Int32 , fromPoint3i, toPoint3i)
IsoJSON(Point3f, V3 Float , fromPoint3f, toPoint3f)
IsoJSON(Point3d, V3 Double, fromPoint3d, toPoint3d)
IsoJSON(Size2i , V2 Int32 , fromSize2i , toSize2i )
IsoJSON(Size2f , V2 Float , fromSize2f , toSize2f )

instance ToJSON (Mat shape channels depth) where
    toJSON = toJSON . matToHMat

instance ( Convert (Proxy shape)    (DS [DS Int32])
         , Convert (Proxy channels) (DS Int32)
         , Convert (Proxy depth)    (DS MatDepth)
         )
      => FromJSON (Mat shape channels depth) where
    parseJSON value = do
      matDyn <- hMatToMat <$> parseJSON value
      case coerceMat (matDyn :: Mat 'D 'D 'D) of
        Left errors -> fail $ intercalate "\n" errors
        Right mat   -> pure mat


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
