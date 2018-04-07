{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Server-side X11 protocol implementation.

module Hex
  ( runServer
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.IO.Unlift (askRunInIO, MonadUnliftIO)
import           Control.Monad.Logger.CallStack (logError, logDebug, logInfo, MonadLogger)
import           Control.Monad.Trans.Reader
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as Network
import           Data.Monoid
import qualified Data.Text as T
import           Data.Word
import           Hex.Parsers
import           Hex.Types

--------------------------------------------------------------------------------
-- Exposed functions

-- | Runs a server in the current thread.
runServer :: (MonadLogger m, MonadUnliftIO m) => m ()
runServer =
  withBound
    (Network.runGeneralTCPServer
       (Network.serverSettings (fst x11PortRange) "127.0.0.1")
       (\app -> do
          logInfo ("New connection from " <> T.pack (show (Network.appSockAddr app)))
          runConduit
            (Network.appSource app .|
             clientMessageConduit .|
             clientMessageSink)
          logInfo ("Connection closed: " <> T.pack (show (Network.appSockAddr app)))))

----------------------------------------------------------------------
-- Threading helpers

-- | Run an action in a bound thread. This is neccessary due to the
-- interaction with signals in C libraries and GHC's runtime.
withBound :: MonadUnliftIO m => m a -> m a
withBound m = do
  run <- askRunInIO
  liftIO (Async.withAsyncBound (run m) Async.wait)

--------------------------------------------------------------------------------
-- Complete protocol conduit

-- | Consumer of client messages and source of server messages.
clientMessageSink ::
     MonadLogger m => ConduitT (Either CA.ParseError ClientMessage) Void m ()
clientMessageSink =
  CL.mapM (\msg -> logDebug (T.pack (show msg))) .| CL.sinkNull

-- | Entry point to the main incoming stream conduit.
clientMessageConduit ::
     MonadLogger m => ConduitT ByteString (Either CA.ParseError ClientMessage) m ()
clientMessageConduit = do
  endiannessResult <- CA.sinkParserEither endiannessParser
  case endiannessResult of
    Left _ -> logError "Invalid endianness."
    Right endianness -> do
      logDebug ("Endianness: " <> T.pack (show endianness))
      let streamSettings =
            StreamSettings {streamSettingsEndianness = endianness}
      initResult <-
        CA.sinkParserEither
          (runReaderT (runStreamParser initiationParser) streamSettings)
      case initResult of
        Left _ -> logError "Invalid initiation message."
        Right version ->
          do logDebug
               ("Received initiation message. Protocol version: " <>
                T.pack (show version))

--------------------------------------------------------------------------------
-- Initial server info

defaultInfo :: Info
defaultInfo =
  Info
  { infoRelease = 0 -- TODO: ?
  , infoResourceIdBase = 0 -- TODO: ?
  , infoResourceIdMask = 0 -- TODO: ?
  , infoMotionBufferSize = 0
  , infoVendor = "Chris Done"
  , infoMaximumRequestLength = maxBound :: Word16
  , infoImageByteOrder = MostSignificantFirst -- TODO: ?
  , infoImageBitOrder = MostSignificantFirst -- TODO: ?
  , infoBitmapFormatScanlineUnit = 32
  , infoBitmapFormatScanlinePad = 32
  , infoMinKeycode = 8
  , infoMaxKeycode = 8
  , infoPixmapFormats = []
  , infoScreens = []
  , infoVersion = Version {versionMajor = 11, versionMinor = 0}
  }

--------------------------------------------------------------------------------
-- Constants

-- | The X11 range of ports. From <https://wiki.wireshark.org/X11>
--
-- > TCP: X11 uses TCP as its transport protocol. The well known TCP
-- > ports for X11 are 6000-6063: typically the port number used is 6000
-- > plus the server/display number.
--
x11PortRange :: (Int, Int)
x11PortRange = (6000, 6063)
