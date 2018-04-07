{-# LANGUAGE LambdaCase #-}

-- | Printers of server messages.

module Hex.Builders where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Coerce
import           Data.Set (Set)
import           Data.Word
import           Hex.Types

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
    , buildByteString (infoVendor info)
    , mconcat (map buildPixmapFormat (infoPixmapFormats info))
    ]
  where
    replyLength = undefined

buildPixmapFormat :: Format -> StreamBuilder
buildPixmapFormat fmt =
  mconcat
    [ buildWord8 (formatDepth fmt)
    , buildWord8 (formatBitsperpixel fmt)
    , buildWord8 (formatScanlinepad fmt)
    , buildUnused 4
    ]

buildScreen :: Screen -> StreamBuilder
buildScreen scr =
  mconcat
    [ buildWord32 (coerce (screenRoot scr))
    , buildWord32 (coerce (screenDefaultColormap scr))
    , buildWord32 (screenWhitePixel scr)
    , buildWord32 (screenBlackPixel scr)
    , buildBitset (screenCurrentInputMasks scr)
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

buildBitset :: Set Event -> StreamBuilder
buildBitset = undefined

buildVersion :: Version -> StreamBuilder
buildVersion (Version major minor) =
  mconcat [buildWord16 major, buildWord16 minor]

buildUnused :: Int -> StreamBuilder
buildUnused n = StreamBuilder (const (L.byteString (S.replicate n 0)))

buildEnum :: Enum a => a -> StreamBuilder
buildEnum = buildWord8 . fromIntegral . fromEnum

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
