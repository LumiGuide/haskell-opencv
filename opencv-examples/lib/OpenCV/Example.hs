{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
{-# language TemplateHaskell #-}

module OpenCV.Example
   ( createCapture
   , createCaptureArg
   , withEmbededFile
   ) where

import "base" System.Environment
import "base" System.Exit
import qualified "bytestring" Data.ByteString as B
import "file-embed" Data.FileEmbed
import "filepath" System.FilePath.Posix
import qualified "opencv" OpenCV as CV
import "template-haskell" Language.Haskell.TH.Syntax
import "temporary" System.IO.Temp

createCapture :: String  -> IO (Maybe CV.VideoCapture)
createCapture input = do
    cap <- CV.newVideoCapture
    let openSource src = CV.exceptErrorIO $ CV.videoCaptureOpen cap src
    if input `elem` ["0","1","2","3","4","5","6","7","8","9"]
      then openSource $ CV.VideoDeviceSource (read input) Nothing
      else openSource $ CV.VideoFileSource input Nothing
    isOpened <- CV.videoCaptureIsOpened cap
    if isOpened
      then return $ Just cap
      else return Nothing

-- | Use first argument to the program to create capture device
-- it should be usefull for creating demos. Numbers are for cameras and
-- files for videos
createCaptureArg :: IO CV.VideoCapture
createCaptureArg =
   getArgs >>= \case
      []      -> createCapture "0"
      (inp:_) -> createCapture inp
    >>= maybe (die "could not open capture input ") return

embedCallHelp :: B.ByteString -> FilePath -> (FilePath -> IO a) -> IO a
embedCallHelp ef templ f =
    withSystemTempFile templ $ \fn fh -> do
      B.hPutStr fh ef
      f fn

-- | Embedder for strictly used files, it allows to embed a file, and still allow
-- some method to use filename
withEmbededFile :: FilePath -> Q Exp
withEmbededFile filePath = do
    templ <- [| $(lift $ takeFileName filePath) |]
    ef <- embedFile filePath
    var <- newName "f"
    return $ LamE [VarP var] $
      AppE (AppE ( AppE (VarE 'embedCallHelp) ef ) templ ) (VarE var)
