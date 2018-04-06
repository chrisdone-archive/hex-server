{-# LANGUAGE OverloadedStrings #-}

-- | A X11 server which opens an SDL window in the default windowing
-- environment, renders graphics onto the SDL surface, and receives
-- input/events from the SDL interface.
--
-- Similar to xnest.

module Main
  ( main
  ) where

import           Control.Monad.Logger
import qualified Hex

-- | Main entry point.
main :: IO ()
main = runStdoutLoggingT Hex.runServer
