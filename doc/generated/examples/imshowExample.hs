import Control.Exception ( bracket )
import Control.Monad ( void )
import qualified OpenCV as CV
import qualified Data.ByteString as B

main :: IO ()
main = do
  img <- CV.imdecode CV.ImreadUnchanged <$> B.readFile "madara.jpg"
  CV.withWindow "Test" $ \window -> do
    CV.imshow window img
    void $ CV.waitKey 10000
