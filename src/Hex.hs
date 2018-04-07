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
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import qualified Data.Attoparsec.Binary as Atto
import qualified Data.Attoparsec.ByteString as Atto
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Conduit
import           Data.Conduit (runConduit, (.|))
import qualified Data.Conduit.Attoparsec as CA
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as Network
import           Data.Functor
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Text as T
import           Data.Word

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
             CL.mapM (\msg -> logDebug (T.pack (show msg))) .|
             CL.sinkNull)
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
-- Messages

-- | Some message from the client to the server.
data ClientMessage =
  EndiannessClientMessage !Endianness
  deriving (Show, Eq, Ord)

-- | Some message from the server to the client.
data ServerMessage =
  ConnectionAccepted !Info
  deriving (Show, Eq, Ord)

-- | Info sent from the server upon successful connection.
data Info = Info
  { infoVersion :: !Version
  , infoRelease :: !Word32
  , infoResourceIdBase :: !Word32
  , infoResourceIdMask :: !Word32
  , infoMotionBufferSize :: !Word32
  , infoVendor :: !ByteString
  , infoMaximumRequestLength :: !Word16
  , infoImageByteOrder :: !Endianness
  , infoImageBitOrder :: !Endianness
  , infoBitmapFormatScanlineUnit :: !Word8
  , infoBitmapFormatScanlinePad :: !Word8
  , infoMinKeycode :: !Word8
  , infoMaxKeycode :: !Word8
  , infoPixmapFormats :: ![Format]
  , infoScreens :: ![Screen]
  } deriving (Show, Eq, Ord)

-- | Pixmap format.
data Format = Format
  { formatDepth :: !Word8
  , formatBitsperpixel :: !Word8
  , formatScanlinepad :: !Word8
  } deriving (Show, Eq, Ord)

-- | A screen/root.
data Screen = Screen
  { screenRoot :: WindowID
  , screenDefaultColormap :: ColorMapID
  , screenWhitePixel :: Word32
  , screenBlackPixel :: Word32
  , screenCurrentInputMasks :: Set Event
  , screenWidthInPixels :: Word16
  , screenHeightInPixels :: Word16
  , screenWidthInMillimeters :: Word16
  , screenHeightInMillimeters :: Word16
  , screenMinInstalledMaps :: Word16
  , screenMaxInstalledMaps :: Word16
  , screenRootVisual :: VisualID
  , screenBackingStores :: BackingStores
  , screenSaveUnders :: Bool
  , screenRootDepth :: Word8
  , screenAllowedDepths :: [Depth]
  } deriving (Show, Eq, Ord)

data BackingStores
  = Never
  | WhenMapped
  | Always
  deriving (Show, Eq, Ord, Enum)

data Event
  = KeyPressEvent
  | KeyReleaseEvent
  | ButtonPressEvent
  | ButtonReleaseEvent
  | EnterWindowEvent
  | LeaveWindowEvent
  | PointerMotionEvent
  | PointerMotionHintEvent
  | Button1MotionEvent
  | Button2MotionEvent
  | Button3MotionEvent
  | Button4MotionEvent
  | Button5MotionEvent
  | ButtonMotionEvent
  | KeymapStateEvent
  | ExposureEvent
  | VisibilityChangeEvent
  | StructureNotifyEvent
  | ResizeRedirectEvent
  | SubstructureNotifyEvent
  | SubstructureRedirectEvent
  | FocusChangeEvent
  | PropertyChangeEvent
  | ColormapChangeEvent
  | OwnerGrabButtonEvent
  deriving (Show, Eq, Ord, Enum)

data Depth = Depth
  { depthDepth :: !Word8
  , depthVisuals :: ![Visual]
  } deriving (Show, Eq, Ord)

data Visual = Visual
  { visualId :: VisualID
  , visualClass :: ColorClass
  , visualBitsPerRgbValue :: Word8
  , visualColormapEntries :: Word16
  , visualRedMask :: Word32
  } deriving (Show, Eq, Ord)

data ColorClass
  = StaticGray
  | StaticColor
  | TrueColor
  | GrayScale
  | PseudoColor
  | DirectColor
  deriving (Show, Eq, Ord)

newtype ResourceID = ResourceID
  { resourceID :: Word32
  } deriving (Show, Eq, Ord)

newtype WindowID = WindowID
  { windowID :: ResourceID
  } deriving (Show, Eq, Ord)

newtype VisualID = VisualID
  { visualID :: ResourceID
  } deriving (Show, Eq, Ord)

newtype ColorMapID = ColorMapID
  { colorMapID :: ResourceID
  } deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Complete protocol conduit

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
          logDebug
            ("Received initiation message. Protocol version: " <>
             T.pack (show version))

--------------------------------------------------------------------------------
-- Endianness parser
--
-- The client must send an initial byte of data to identify the byte
-- order to be employed. The value of the byte must be octal 102 or
-- 154. The value 102 (ASCII uppercase B) means values are transmitted
-- most significant byte first, and value 154 (ASCII lowercase l)
-- means values are transmitted least significant byte first. Except
-- where explicitly noted in the protocol, all 16-bit and 32-bit
-- quantities sent by the client must be transmitted with this byte
-- order, and all 16-bit and 32-bit quantities returned by the server
-- will be transmitted with this byte order.

-- | Endianness of the connection stream.
data Endianness
  = MostSignificantFirst
  | LeastSignificantFirst
  deriving (Show, Eq, Ord, Enum)

-- | Parse the endianness.
endiannessParser :: Parser Endianness
endiannessParser = Atto.choice [most, least] <* unused
  where
    least = LeastSignificantFirst <$ Atto8.char 'l'
    most = MostSignificantFirst <$ Atto8.char 'B'
    unused = void Atto.anyWord8

--------------------------------------------------------------------------------
-- Parsers for X11-protocol-specific types

-- | X11-specific settings.
data StreamSettings = StreamSettings
  { streamSettingsEndianness :: !Endianness
  } deriving (Show)

-- | A stream parser that holds some X11-specific settings.
newtype StreamParser a = StreamParser
  { runStreamParser :: ReaderT StreamSettings Parser a
  } deriving (Monad, Functor, Applicative)

-- | Protocol version.
data Version = Version
  { versionMajor, versionMinor :: Word16
  } deriving (Show, Eq, Ord)

-- | An unused number of bytes.
unusedParser :: Int -> StreamParser ()
unusedParser n = StreamParser (lift (void (Atto.take n)))

-- | Parse minor/major versions.
protocolVersionParser :: StreamParser Version
protocolVersionParser = do
  major <- card16Parser
  minor <- card16Parser
  pure (Version {versionMajor = major, versionMinor = minor})

-- | Connection initiation. The data is ignored, we just walk past it.
initiationParser :: StreamParser Version
initiationParser = do
  version <- protocolVersionParser
  authNameLen <- stringLengthParser
  authDataLen <- stringLengthParser
  unusedParser 2
  _authName <- stringParser authNameLen
  _authData <- stringParser authDataLen
  pure version

-- | Parse a length of a string.
stringLengthParser :: StreamParser Word16
stringLengthParser = card16Parser

-- | Parse a string, including padding.
stringParser :: Word16 -> StreamParser ByteString
stringParser len =
  StreamParser (lift (fmap (S.take (fromIntegral len)) (Atto.take (pad len))))

-- | Parse a 16-bit word with the right endianness.
card16Parser :: StreamParser Word16
card16Parser =
  StreamParser
    (do endianness <- asks streamSettingsEndianness
        lift
          (case endianness of
             MostSignificantFirst -> Atto.anyWord16be
             LeastSignificantFirst -> Atto.anyWord16le))

--------------------------------------------------------------------------------
-- X11 Helpers

-- | If the number of unused bytes is variable, the encode-form
-- typically is:
--
-- p unused, p=pad(E)
--
-- where E is some expression, and pad(E) is the number of bytes
-- needed to round E up to a multiple of four.
--
-- pad(E) = (4 - (E mod 4)) mod 4
pad :: Word16 -> Int
pad e =
  fromIntegral
    (case mod e 4 of
       0 -> e
       remainder -> e + 4 - remainder)

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
