{-# language LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenCV.Example
 ( createCapture
 , createCaptureArg
 , withEmbededFile
 )
where

import qualified OpenCV as CV
import System.Environment
import System.Exit
import System.FilePath.Posix

import System.IO.Temp
import Data.FileEmbed

import Language.Haskell.TH.Syntax
import qualified Data.ByteString as B

createCapture :: String  -> IO (Maybe CV.VideoCapture)
createCapture input = do
    cap <- CV.newVideoCapture
    if input `elem` ["0","1","2","3","4","5","6","7","8","9"]
      then
         CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoDeviceSource (read input) Nothing
      else
         CV.exceptErrorIO $ CV.videoCaptureOpen cap $ CV.VideoFileSource input Nothing
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

-- | Embeder for strictly used files, it allows to embed a file, and still allow
-- some method to use filename
withEmbededFile :: FilePath -> Q Exp
withEmbededFile filePath = do
  templ <- [| $(lift $ takeFileName filePath) |]
  ef <- embedFile filePath
  var <- newName "f"
  return $ LamE [VarP var] $
    AppE (AppE ( AppE (VarE 'embedCallHelp) ef ) templ ) (VarE var)
