{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.

module Hex.Types where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Reader (ReaderT)
import           Data.Attoparsec.ByteString (Parser)
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy.Builder (Builder)
import qualified Data.Conduit.Attoparsec as CA
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable (Hashable)
import           Data.Set (Set)
import           Data.Typeable
import           Data.Word

--------------------------------------------------------------------------------
-- Client state

-- | State for each client.
data ClientState = ClientState
  { clientStateSequenceNumber :: !SequenceNumber
  , clientStateAtoms :: !(HashMap AtomID ByteString)
  } deriving (Show)

--------------------------------------------------------------------------------
-- Parsers and printers

-- | X11-specific settings.
data StreamSettings = StreamSettings
  { streamSettingsEndianness :: !Endianness
  } deriving (Show)

-- | A stream builder that accepts X11-specific settings.
newtype StreamBuilder = StreamBuilder
  { runStreamBuilder :: StreamSettings -> Builder
  } deriving (Monoid)

-- | A stream parser that holds some X11-specific settings.
newtype StreamParser a = StreamParser
  { runStreamParser :: ReaderT StreamSettings Parser a
  } deriving (Monad, Functor, Applicative, Alternative)

--------------------------------------------------------------------------------
-- Exceptions and errors

-- | A fatal problem with the handshake.
data ClientException
  = InvalidEndianness CA.ParseError
  | InvalidInitiationMessage CA.ParseError
  | InvalidRequest CA.ParseError
  deriving (Show, Typeable)

instance Exception ClientException where
  displayException =
    \case
      InvalidEndianness e ->
        "Invalid endianness in X11 handshake. Parse error was: " ++ show e
      InvalidInitiationMessage e ->
        "Invalid endianness in X11 handshake. Parse error was: " ++ show e
      InvalidRequest e ->
        "Invalid request in X11 request/reply loop. Parse error was: " ++
        show e

--------------------------------------------------------------------------------
-- Messages

-- | Some message from the client to the server.
data ClientMessage
  = QueryExtension !ByteString
  | CreateGC
  | FreeGC
  | GetProperty
  | CreateWindow
  | XCMiscGetXIDRange
  | InternAtom !ByteString !Bool
  | ChangeProperty
  deriving (Show, Eq, Ord)

-- | Some message from the server to the client.
data ServerMessage
  = ConnectionAccepted !Info
  | UnsupportedExtension !SequenceNumber
  | PropertyValue !SequenceNumber
  | SupportedExtension !SequenceNumber !Opcode
  | XIDRange !SequenceNumber
  | AtomInterned !SequenceNumber !AtomID
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
  deriving (Show, Eq, Ord, Enum)

newtype ResourceID = ResourceID
  { resourceID :: Word32
  } deriving (Show, Eq, Ord)

newtype AtomID = AtomID
  { atomID :: Word32
  } deriving (Show, Eq, Ord, Hashable, Num, Real, Enum)

newtype WindowID = WindowID
  { windowID :: ResourceID
  } deriving (Show, Eq, Ord)

newtype VisualID = VisualID
  { visualID :: ResourceID
  } deriving (Show, Eq, Ord)

newtype ColorMapID = ColorMapID
  { colorMapID :: ResourceID
  } deriving (Show, Eq, Ord)

-- | Protocol version.
data Version = Version
  { versionMajor, versionMinor :: Word16
  } deriving (Show, Eq, Ord)

-- | Endianness of something.
data Endianness
  = LeastSignificantFirst
  | MostSignificantFirst
  deriving (Show, Eq, Ord, Enum)

-- | Result of connection setup.
data SetupResult
  = Failed
  | Success
  | Authenticate
  deriving (Show, Eq, Ord, Enum)

newtype SequenceNumber = SequenceNumber
  { sequenceNumber :: Word16
  } deriving (Show, Eq, Ord, Num, Integral, Real, Enum)

newtype Opcode = Opcode
  { opcode :: Word8
  } deriving (Show, Eq, Ord, Num, Integral, Real, Enum)
