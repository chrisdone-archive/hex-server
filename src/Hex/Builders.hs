{-# LANGUAGE LambdaCase #-}

-- | Printers of server messages.

module Hex.Builders where

import qualified Data.ByteString as S
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Builder as L
import           Data.Word
import           Hex.Types

buildServerMessage :: ServerMessage -> StreamBuilder
buildServerMessage =
  \case
    ConnectionAccepted info ->
      mconcat [buildSetupResult Success, buildInfo info]

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
buildScreen = undefined

buildVersion :: Version -> StreamBuilder
buildVersion (Version major minor) =
  mconcat [buildWord16 major, buildWord16 minor]

buildSetupResult :: SetupResult -> StreamBuilder
buildSetupResult = buildEnum

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
