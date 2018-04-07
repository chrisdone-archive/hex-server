{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All types.

module Hex.Types where

import Control.Monad.Reader (ReaderT)
import Data.Attoparsec.ByteString (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder)
import Data.Set (Set)
import Data.Word

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
  } deriving (Monad, Functor, Applicative)

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
  deriving (Show, Eq, Ord, Enum)

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
