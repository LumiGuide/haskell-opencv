module Main ( main ) where

import qualified "attoparsec" Data.Attoparsec.ByteString as A
import "bytestring" Data.ByteString ( ByteString )
import qualified "bytestring" Data.ByteString as B
import qualified "bytestring" Data.ByteString.Unsafe as B ( unsafeHead  )
import "conduit" Data.Conduit
    ( Conduit, Sink, Source
    , unwrapResumable, addCleanup
    , ($=+), ($$+-), await
    , mapOutput
    , ZipSource(..), getZipSource
    )
import "conduit" Data.Conduit.Internal ( ResumableSource(ResumableSource), Pipe(Done), unConduitM )
import "conduit-extra" Data.Conduit.Attoparsec ( conduitParser )
import "exceptions" Control.Monad.Catch ( MonadThrow )
import qualified "http-conduit" Network.HTTP.Conduit as Http
import "lumi-hackage-extended" Lumi.Prelude hiding ( yield )
import qualified "optparse-applicative" Options.Applicative as O
import "resourcet" Control.Monad.Trans.Resource ( ResourceT, runResourceT )
import "transformers" Control.Monad.IO.Class ( MonadIO, liftIO )

import "thea" OpenCV

--------------------------------------------------------------------------------

type Jpeg = ByteString

--------------------------------------------------------------------------------

axisUrlL :: String
axisUrlL = "http://192.168.42.4/axis-cgi/mjpg/video.cgi?resolution=1280x800&compression=15&fps=30&rotation=0&source=1"

axisUrlR :: String
axisUrlR = "http://192.168.42.70/axis-cgi/mjpg/video.cgi?resolution=1280x800&compression=15&fps=30&rotation=0&source=1"

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
thea _opts = do
  let mbReqs =
        (,) <$> (Http.applyBasicAuth axisUsername axisPassword <$> Http.parseUrl axisUrlL)
            <*> (Http.applyBasicAuth axisUsername axisPassword <$> Http.parseUrl axisUrlR)
  case mbReqs of
    Just (reqL, reqR) -> withRequest reqL reqR
    Nothing -> error "wrong request"

withRequest :: Http.Request -> Http.Request -> IO ()
withRequest reqL reqR = do
    manager <- Http.newManager Http.tlsManagerSettings
    runResourceT $ do
      respL <- Http.http reqL manager
      respR <- Http.http reqR manager
      rSrc <- zipResumableSources (Http.responseBody respL $=+ mjpegConduit)
                                  (Http.responseBody respR $=+ mjpegConduit)
      window <- liftIO $ makeWindow "output" <* threadDelay 20000
      rSrc $$+- jpegSink window
      liftIO $ destroyWindow window >> threadDelay 10000

jpegSink :: Window -> Sink (Jpeg, Jpeg) (ResourceT IO) ()
jpegSink window = do
    fix $ \go -> do
      mbJpegs <- await
      forM_ mbJpegs $ \jpegs -> do
        continue <- liftIO $ handleJpeg window jpegs
        when continue go

handleJpeg :: Window -> (Jpeg, Jpeg) -> IO Bool
handleJpeg window (jpegL, jpegR) = do
    let imgL = createMat $ imdecodeM jpegL ImreadColor
        imgR = createMat $ imdecodeM jpegR ImreadColor
        blend = either throw id $ addWeighted imgL 0.5 imgR 0.5 0.0
    imshow window blend
    (/= 27) <$> waitKey 5

zipResumableSources :: forall m a b. (MonadIO m) => ResumableSource m a -> ResumableSource m b -> m (ResumableSource m (a,b))
zipResumableSources rSrc1 rSrc2 = do
    (src1, finalize1) <- unwrapResumable rSrc1
    (src2, finalize2) <- unwrapResumable rSrc2

    let src :: Source m (a, b)
        src = getZipSource $ (,) <$> ZipSource src1 <*> ZipSource src2

        finalize :: m ()
        finalize = finalize1 *> finalize2

        rSrc :: ResumableSource m (a, b)
        rSrc = ResumableSource (flip unConduitM Done $ addCleanup (\_isCompleted -> finalize) src) finalize

    pure rSrc
