-- | Parsers of client messages.

module Hex.Parsers
  (endiannessParser
  ,initiationParser
  ,requestParser)
  where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import qualified Data.Attoparsec.Binary as Atto
import qualified Data.Attoparsec.ByteString as Atto
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto8
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Coerce
import           Data.Functor
import           Data.Word
import           Hex.Constants
import           Hex.Types

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

-- | Parse the endianness.
endiannessParser :: Parser Endianness
endiannessParser = Atto.choice [most, least] <* unused
  where
    least = LeastSignificantFirst <$ Atto8.char 'l'
    most = MostSignificantFirst <$ Atto8.char 'B'
    unused = void Atto.anyWord8

--------------------------------------------------------------------------------
-- Initiation parsers

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

--------------------------------------------------------------------------------
-- Requests

-- | Parse client requests.
requestParser :: StreamParser ClientMessage
requestParser =
  (QueryExtension <$> queryExtensionParser) <|> (CreateGC <$ createGCParser) <|>
  (GetProperty <$ getPropertyParser) <|>
  (CreateWindow <$ createWindowParser) <|>
  (XCMiscGetXIDRange <$ xcMiscGetXIDRangeParser) <|>
  (uncurry InternAtom <$> internAtomParser)

-- | QueryExtension: This request determines if the named extension is
-- present.
queryExtensionParser :: StreamParser ByteString
queryExtensionParser = do
  opcodeParser8 queryExtensionOpcode
  unusedParser 1
  _reqlen <- remainingRequestLength
  nameLen <- stringLengthParser
  unusedParser 2
  stringParser nameLen

-- | CreateGC.
createGCParser :: StreamParser ()
createGCParser = do
  opcodeParser8 createGCOpcode
  unusedParser 1
  reqlen <- remainingRequestLength
  unusedParser reqlen

-- | GetProperty.
getPropertyParser :: StreamParser ()
getPropertyParser = do
  opcodeParser8 getPropertyOpcode
  unusedParser 1 -- TODO: delete
  reqlen <- remainingRequestLength
  unusedParser reqlen

-- | CreateWindow.
createWindowParser :: StreamParser ()
createWindowParser = do
  opcodeParser8 createWindowOpcode
  unusedParser 1 -- depth
  reqlen <- remainingRequestLength
  unusedParser reqlen

-- | XCMiscGetXIDRange.
xcMiscGetXIDRangeParser :: StreamParser ()
xcMiscGetXIDRangeParser = do
  opcodeParser8 xcMiscOpcode
  opcodeParser8 xcMiscGetXIDRangeOpcode
  unusedParser 2

-- | InternAtom.
internAtomParser :: StreamParser (ByteString, Bool)
internAtomParser = do
  opcodeParser8 internAtomOpcode
  onlyIfExists <- enumParser
  _len <- remainingRequestLength
  nameLen <- stringLengthParser
  unusedParser 2
  name <- stringParser nameLen
  pure (name, onlyIfExists)

--------------------------------------------------------------------------------
-- Parsers for X11-protocol-specific types

-- | Request lengths are multiples of 4. So if the whole request size
-- in bytes is 20, then the value will be 5 (5x4=20). To remove the
-- header of 4 bytes we just subtract 4.
remainingRequestLength :: StreamParser Int
remainingRequestLength = do
  factorOf4 <- card16Parser
  pure ((fromIntegral factorOf4 * 4) - 4)

-- | An unused number of bytes.
unusedParser :: Int -> StreamParser ()
unusedParser n = StreamParser (lift (void (Atto.take n)))

-- | Parse a length of a string.
stringLengthParser :: StreamParser Word16
stringLengthParser = card16Parser

-- | Parse a string, including padding.
stringParser :: Word16 -> StreamParser ByteString
stringParser len =
  StreamParser (lift (fmap (S.take (fromIntegral len)) (Atto.take (pad len))))

-- | Parse a string, including padding.
enumParser :: Enum e => StreamParser e
enumParser =
  StreamParser (lift (fmap (toEnum . fromIntegral) (Atto.anyWord8))) -- TODO: Safe toEnum.

-- | An byte number of bytes.
opcodeParser8 :: Opcode -> StreamParser ()
opcodeParser8 n = StreamParser (lift (void (Atto.word8 (coerce n))))

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
