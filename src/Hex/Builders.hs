{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Printers of server messages.

module Hex.Builders where

import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Coerce
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word
import           Hex.Types

streamBuilderToByteString :: StreamSettings -> StreamBuilder -> ByteString
streamBuilderToByteString settings builder =
  L.toStrict (L.toLazyByteString (runStreamBuilder builder settings))

buildServerMessage :: ServerMessage -> StreamBuilder
buildServerMessage =
  \case
    ConnectionAccepted info ->
      mconcat [buildEnum Success, buildInfo info]

buildInfo :: Info -> StreamBuilder
buildInfo info =
  mconcat
    [ buildUnused 1
    , buildVersion (infoVersion info)
    , buildWord16 replyLength
    , buildWord32 (infoRelease info)
    , buildWord32 (infoResourceIdBase info)
    , buildWord32 (infoResourceIdMask info)
    , buildWord32 (infoMotionBufferSize info)
    , buildWord16 (fromIntegral (S.length (infoVendor info)))
    , buildWord16 (infoMaximumRequestLength info)
    , buildWord8 (fromIntegral (length (infoScreens info)))
    , buildWord8 (fromIntegral (length (infoPixmapFormats info)))
    , buildEnum (infoImageByteOrder info)
    , buildEnum (infoImageBitOrder info)
    , buildWord8 (infoBitmapFormatScanlineUnit info)
    , buildWord8 (infoBitmapFormatScanlinePad info)
    , buildWord8 (infoMinKeycode info)
    , buildWord8 (infoMaxKeycode info)
    , buildUnused 4
    , buildByteStringPadded (infoVendor info)
    , mconcat (map buildPixmapFormat (infoPixmapFormats info))
    , mconcat (map buildScreen (infoScreens info))
    ]
  where
    replyLength = 8 + 2 * n + ((p + m) `div` 4)
      where
        n = fromIntegral (length (infoPixmapFormats info))
        v = fromIntegral (S.length (infoVendor info))
        p = pad v
        m = fromIntegral (length (infoScreens info))

buildPixmapFormat :: Format -> StreamBuilder
buildPixmapFormat fmt =
  mconcat
    [ buildWord8 (formatDepth fmt)
    , buildWord8 (formatBitsperpixel fmt)
    , buildWord8 (formatScanlinepad fmt)
    , buildUnused 5
    ]

buildScreen :: Screen -> StreamBuilder
buildScreen scr =
  mconcat
    [ buildWord32 (coerce (screenRoot scr))
    , buildWord32 (coerce (screenDefaultColormap scr))
    , buildWord32 (screenWhitePixel scr)
    , buildWord32 (screenBlackPixel scr)
    , buildEventSet (screenCurrentInputMasks scr)
    , buildWord16 (screenWidthInPixels scr)
    , buildWord16 (screenHeightInPixels scr)
    , buildWord16 (screenWidthInMillimeters scr)
    , buildWord16 (screenHeightInMillimeters scr)
    , buildWord16 (screenMinInstalledMaps scr)
    , buildWord16 (screenMaxInstalledMaps scr)
    , buildWord32 (coerce (screenRootVisual scr))
    , buildEnum (screenBackingStores scr)
    , buildEnum (screenSaveUnders scr)
    , buildWord8 (screenRootDepth scr)
    , buildWord8 (fromIntegral (length (screenAllowedDepths scr)))
    , mconcat (map buildDepth (screenAllowedDepths scr))
    ]

buildDepth :: Depth -> StreamBuilder
buildDepth depth =
  mconcat
    [ buildWord8 (depthDepth depth)
    , buildUnused 1
    , buildWord16 (fromIntegral (length (depthVisuals depth)))
    , buildUnused 4
    , mconcat (map buildVisual (depthVisuals depth))
    ]

buildVisual :: Visual -> StreamBuilder
buildVisual v =
  mconcat
    [ buildWord32 (coerce (visualId v))
    , buildEnum (visualClass v)
    , buildWord8 (visualBitsPerRgbValue v)
    , buildWord16 (visualColormapEntries v)
    , buildWord32 (visualRedMask v)
    ]

buildEventSet :: Set Event -> StreamBuilder
buildEventSet = buildWord32 . foldl (.|.) 0 . map encode . Set.toList
  where
    encode :: Event -> Word32
    encode =
      \case
        KeyPressEvent -> 0x00000001
        KeyReleaseEvent -> 0x00000002
        ButtonPressEvent -> 0x00000004
        ButtonReleaseEvent -> 0x00000008
        EnterWindowEvent -> 0x00000010
        LeaveWindowEvent -> 0x00000020
        PointerMotionEvent -> 0x00000040
        PointerMotionHintEvent -> 0x00000080
        Button1MotionEvent -> 0x00000100
        Button2MotionEvent -> 0x00000200
        Button3MotionEvent -> 0x00000400
        Button4MotionEvent -> 0x00000800
        Button5MotionEvent -> 0x00001000
        ButtonMotionEvent -> 0x00002000
        KeymapStateEvent -> 0x00004000
        ExposureEvent -> 0x00008000
        VisibilityChangeEvent -> 0x00010000
        StructureNotifyEvent -> 0x00020000
        ResizeRedirectEvent -> 0x00040000
        SubstructureNotifyEvent -> 0x00080000
        SubstructureRedirectEvent -> 0x00100000
        FocusChangeEvent -> 0x00200000
        PropertyChangeEvent -> 0x00400000
        ColormapChangeEvent -> 0x00800000
        OwnerGrabButtonEvent -> 0x01000000

buildVersion :: Version -> StreamBuilder
buildVersion (Version major minor) =
  mconcat [buildWord16 major, buildWord16 minor]

buildUnused :: Int -> StreamBuilder
buildUnused n = StreamBuilder (const (L.byteString (S.replicate n 0)))

buildEnum :: Enum a => a -> StreamBuilder
buildEnum = buildWord8 . fromIntegral . fromEnum

buildByteStringPadded :: ByteString -> StreamBuilder
buildByteStringPadded = StreamBuilder . const . L.byteString . padded
  where
    padded s =
      S.take
        (fromIntegral (pad (fromIntegral (S.length s))))
        (s <> S.replicate 4 (0 :: Word8))

buildByteString :: ByteString -> StreamBuilder
buildByteString = StreamBuilder . const . L.byteString

buildWord8 :: Word8 -> StreamBuilder
buildWord8 = StreamBuilder . const . L.word8

buildWord16 :: Word16 -> StreamBuilder
buildWord16 w =
  StreamBuilder
    (\s ->
       case streamSettingsEndianness s of
         MostSignificantFirst -> L.word16BE w
         LeastSignificantFirst -> L.word16LE w)

buildWord32 :: Word32 -> StreamBuilder
buildWord32 w =
  StreamBuilder
    (\s ->
       case streamSettingsEndianness s of
         MostSignificantFirst -> L.word32BE w
         LeastSignificantFirst -> L.word32LE w)

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
pad :: Word16 -> Word16
pad e =
  case mod e 4 of
    0 -> e
    remainder -> e + 4 - remainder
