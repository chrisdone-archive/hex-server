{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent
import           Control.Concurrent.Async as Async
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift (askRunInIO, MonadUnliftIO)
import           Control.Monad.Logger
import           Data.Functor (void)
import qualified Graphics.X11.Xlib.Display as Xlib
import qualified Hex
import           Test.Hspec

-- | Main entry point.
main :: IO ()
main = hspec spec

-- | Test suite spec.
spec :: SpecWith ()
spec = basic

-- | Basic connection tests.
basic :: SpecWith ()
basic =
  describe
    "Basic"
    (do it
          "Listen & connect"
          (do void
                (withAsync
                   (withBound (runStdoutLoggingT Hex.runServer))
                   (const
                      (withBound
                         (do threadDelay (1000 * 500)
                             void (Xlib.openDisplay "192.168.1.102:0")))))
              shouldBe True True))

-- | Run an action in a bound thread. This is neccessary due to the
-- interaction with signals in C libraries and GHC's runtime.
withBound :: MonadUnliftIO m => m a -> m a
withBound m = do
  run <- askRunInIO
  liftIO (Async.withAsyncBound (run m) Async.wait)
