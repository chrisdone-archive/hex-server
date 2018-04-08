{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Server-side X11 protocol implementation.

module Hex
  ( runServer
  ) where

import           BinaryView
import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import           Control.Monad.IO.Class
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
import           Hex.Builders
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
          logInfo
            ("New connection from " <> T.pack (show (Network.appSockAddr app)))
          runConduit
            (Network.appSource app .|
             CL.mapM
               (\bytes -> do
                  logDebug ("<=\n" <> T.pack (binaryView 1 bytes))
                  pure bytes) .|
             clientMessageConduit .|
             clientMessageSink app)
          logInfo
            ("Connection closed: " <> T.pack (show (Network.appSockAddr app)))))

--------------------------------------------------------------------------------
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
     (MonadIO m, MonadLogger m)
  => Network.AppData
  -> ConduitT ClientMessage Void m ()
clientMessageSink app = do
  mmsg <- await
  case mmsg of
    Nothing -> logError "No initial message."
    Just msg -> do
      logDebug ("=> " <> T.pack (show msg))
      case msg of
        InitialClientMessage endianness ->
          let streamSettings =
                StreamSettings {streamSettingsEndianness = endianness}
          in do runConduit
                  (yield (ConnectionAccepted defaultInfo) .|
                   CL.map
                     (streamBuilderToByteString streamSettings .
                      buildServerMessage) .|
                   CL.mapM
                     (\bytes -> do
                        logDebug ("=>\n" <> T.pack (binaryView 1 bytes))
                        pure bytes) .|
                   Network.appSink app)
                CL.sinkNull

-- | Entry point to the main incoming stream conduit.
clientMessageConduit ::
     MonadLogger m => ConduitT ByteString ClientMessage m ()
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
        Right version -> do
          logDebug
            ("Received initiation message. Protocol version: " <>
             T.pack (show version))
          yield (InitialClientMessage endianness)
          CL.sinkNull

--------------------------------------------------------------------------------
-- Initial server info

defaultInfo :: Info
defaultInfo =
  Info
  { infoRelease = 0 -- TODO: ?
  , infoResourceIdBase = 0 -- TODO: ?
  , infoResourceIdMask = 1 -- TODO: ?
  , infoMotionBufferSize = 0
  , infoVendor = "ABCD"
  , infoMaximumRequestLength = maxBound :: Word16
  , infoImageByteOrder = MostSignificantFirst -- TODO: ?
  , infoImageBitOrder = MostSignificantFirst -- TODO: ?
  , infoBitmapFormatScanlineUnit = 32
  , infoBitmapFormatScanlinePad = 32
  , infoMinKeycode = 8
  , infoMaxKeycode = 8
  , infoPixmapFormats =
      [ Format
        {formatDepth = 32, formatBitsperpixel = 32, formatScanlinepad = 32}
      ]
  , infoScreens =
      [ Screen
        { screenRoot = WindowID (ResourceID 0)
        , screenDefaultColormap = ColorMapID (ResourceID 0)
        , screenWhitePixel = 0x00ffffff
        , screenBlackPixel = 0x00000000
        , screenCurrentInputMasks = mempty
        , screenWidthInPixels = 1024
        , screenHeightInPixels = 768
        , screenWidthInMillimeters = 1024
        , screenHeightInMillimeters = 768
        , screenMinInstalledMaps = 1
        , screenMaxInstalledMaps = 1
        , screenRootVisual = VisualID (ResourceID 0)
        , screenBackingStores = Never
        , screenSaveUnders = False
        , screenRootDepth = 32
        , screenAllowedDepths = [Depth 32 []]
        }
      ]
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
