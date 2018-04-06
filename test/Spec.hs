{-# LANGUAGE OverloadedStrings #-}

import           Control.Concurrent.Async
import           Control.Monad.Logger
import qualified Hex
import           Data.Functor (void)
import qualified Graphics.X11.Xlib.Display as Xlib
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
                (race
                   (runStdoutLoggingT Hex.runServer)
                   (void (Xlib.openDisplay ":0")))
              shouldBe True True))
