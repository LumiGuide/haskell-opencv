import Control.Monad ( void )
import qualified OpenCV as CV
import qualified Data.ByteString as B

main :: IO ()
main = do
    img <- CV.imdecode CV.ImreadUnchanged <$> B.readFile "data/Lenna.png"
    CV.withWindow "test" $ \window -> do
      CV.imshow window img
      void $ CV.waitKey 0
