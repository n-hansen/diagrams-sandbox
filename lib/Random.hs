module Random
  ( runRandom
  , randomHue
  , Random
  , module System.Random.MWC
  ) where

import           Control.Monad.Primitive
import           Data.Colour.RGBSpace.HSV
import           Data.Vector              (fromList)
import           System.Random.MWC

type Random m = ReaderT (Gen (PrimState m)) m

runRandom :: PrimMonad m => [Word32] -> Random m a -> m a
runRandom seed m = do
  g <- initialize $ fromList seed
  runReaderT m g

randomHue :: (Variate a, PrimMonad m, RealFrac a, Floating a) => a -> a -> Random m (Colour a)
randomHue s v = do
  g <- ask
  h <- lift $ uniformR (0, 360) g
  let RGB{channelRed,channelBlue,channelGreen} = hsv h s v
  return $ sRGB channelRed channelGreen channelBlue
