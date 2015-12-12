module Main ( main ) where

import qualified "attoparsec" Data.Attoparsec.ByteString as A
import "bytestring" Data.ByteString ( ByteString )
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Unsafe as B ( unsafeHead  )
import "conduit" Data.Conduit
    ( Conduit, Sink, ResumableSource
    , ($=+), ($$+-), await
    , mapOutput
    )
import "conduit-extra" Data.Conduit.Attoparsec ( conduitParser )
import "exceptions" Control.Monad.Catch ( MonadThrow )
import qualified "http-conduit" Network.HTTP.Conduit as Http
import "lumi-hackage-extended" Lumi.Prelude hiding ( yield )
import qualified "optparse-applicative" Options.Applicative as O
import "resourcet" Control.Monad.Trans.Resource ( ResourceT, runResourceT )
import "transformers" Control.Monad.IO.Class ( liftIO )

import "thea" OpenCV

--------------------------------------------------------------------------------

type Jpeg = ByteString

--------------------------------------------------------------------------------

axisUrl :: String
axisUrl = "http://192.168.42.4/axis-cgi/mjpg/video.cgi?resolution=1280x800&compression=15&fps=30&rotation=0&source=1"

axisUsername :: ByteString
axisUsername = "root"

axisPassword :: ByteString
axisPassword = "lumicam"

--------------------------------------------------------------------------------

-- mjpegConduit :: (Monad m) => Conduit ByteString m Jpeg
-- mjpegConduit = await >>= \mbBs -> forM_ mbBs $ \bs ->
--     let (_beforeStart, fromStart) = B.breakSubstring startMarker bs
--     in if B.null fromStart
--        then mjpegConduit
--        else findStop [] fromStart

-- findStop :: (Monad m) => [ByteString] -> ByteString -> Conduit MJpeg m Jpeg
-- findStop acc bs =
--     let (beforeStop, fromStop) = B.breakSubstring stopMarker bs
--     in if B.null fromStop
--        then await >>= \mbBs -> forM_ mbBs $ findStop (bs:acc)
--        else do let jpg = mconcat $ reverse acc <> [beforeStop, stopMarker]
--                yield jpg
--                let rest = B.drop (B.length stopMarker) fromStop
--                unless (B.null rest) $ leftover rest
--                mjpegConduit

mjpegConduit :: (Monad m, MonadThrow m) => Conduit ByteString m Jpeg
mjpegConduit = mapOutput snd $ conduitParser mjpegParser

mjpegParser :: A.Parser Jpeg
mjpegParser =
    skipUpToAndIncludingString startMarker
    *> ((startMarker <>) <$> takeUpToAndIncludingString stopMarker)
  where
    skipUpToAndIncludingString :: ByteString -> A.Parser ()
    skipUpToAndIncludingString s =
        void (A.string s) <|> (A.anyWord8 *> skipUpToAndIncludingString s)

    takeUpToAndIncludingString :: ByteString -> A.Parser ByteString
    takeUpToAndIncludingString s = do
        xs <- A.takeWhile (/= B.unsafeHead s)
        (A.string s *> pure (xs <> s)) <|> ((\b bs -> xs <> B.cons b bs) <$> A.anyWord8 <*> takeUpToAndIncludingString s)

startMarker, stopMarker :: ByteString
startMarker = "\xff\xd8"
stopMarker  = "\xff\xd9"

--------------------------------------------------------------------------------


main :: IO ()
main = O.execParser opts >>= thea
  where
    opts :: O.ParserInfo Options
    opts = O.info (O.helper <*> options)
      (  O.fullDesc
      <> O.progDesc "A playground for experimenting with Computer Vision"
      )
    options :: O.Parser Options
    options = pure Options

data Options = Options

thea :: Options -> IO ()
thea _opts =
    case Http.parseUrl axisUrl of
      Nothing -> putStrLn "invalid URL"
      Just req -> withRequest $ Http.applyBasicAuth axisUsername axisPassword req

window :: WindowName
window = "output"

withRequest :: Http.Request -> IO ()
withRequest req = do
    manager <- Http.newManager Http.tlsManagerSettings
    runResourceT $ do
      resp <- Http.http req manager
      let bytesSource :: ResumableSource (ResourceT IO) ByteString
          bytesSource = Http.responseBody resp
      liftIO $ do
        makeWindow window
        void $ waitKey 20
      bytesSource $=+ mjpegConduit $$+- jpegSink

jpegSink :: Sink Jpeg (ResourceT IO) ()
jpegSink = do
    mousePosRef <- liftIO $ newIORef (0, 0)
    circleSizeRef <- liftIO $ newIORef initCircleSize
    liftIO $ setMouseCallback window $ \_event x y _flags -> writeIORef mousePosRef (x, y)
    liftIO $ createTrackbar "circle size" window initCircleSize 200 $ writeIORef circleSizeRef
    fix $ \go -> do
      mbJpeg <- await
      forM_ mbJpeg $ \jpeg -> do
        continue <- liftIO $ handleJpeg mousePosRef circleSizeRef jpeg
        when continue go
  where
    initCircleSize = 30

handleJpeg :: IORef (Int, Int) -> IORef Int -> Jpeg -> IO Bool
handleJpeg mousePosRef circleSizeRef jpeg =
    case medianBlur (imdecode jpeg ImreadColor) 3 of
      Left err -> throwIO err
      Right blur -> do
        (mouseX, mouseY) <- readIORef mousePosRef
        circleSize <- readIORef circleSizeRef
        let img = runST $ do
                    imgM <- thaw blur
                    circle imgM
                           (mkPoint2i mouseX mouseY)
                           circleSize
                           (mkScalar 0 0 255.0 0)
                           2
                           LineType_AA
                           0
                    freeze imgM
        imshow window img
        (/= 27) <$> waitKey 5
