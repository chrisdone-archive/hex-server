{-# LANGUAGE OverloadedStrings #-}

-- | Constant values.

module Hex.Constants where

import           Data.ByteString (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Hex.Types

-- | The X11 range of ports. From <https://wiki.wireshark.org/X11>
--
-- > TCP: X11 uses TCP as its transport protocol. The well known TCP
-- > ports for X11 are 6000-6063: typically the port number used is 6000
-- > plus the server/display number.
--
x11PortRange :: (Int, Int)
x11PortRange = (6000, 6063)

initialSequenceNumber :: SequenceNumber
initialSequenceNumber = 1

--------------------------------------------------------------------------------
-- Opcodes

internAtomOpcode :: Opcode
internAtomOpcode = 16

queryExtensionOpcode :: Opcode
queryExtensionOpcode = 98

createGCOpcode :: Opcode
createGCOpcode = 55

freeGCOpcode :: Opcode
freeGCOpcode = 60

getPropertyOpcode :: Opcode
getPropertyOpcode = 20

changePropertyOpcode :: Opcode
changePropertyOpcode = 18

changeWindowAttributesOpcode :: Opcode
changeWindowAttributesOpcode = 2

getWindowAttributesOpcode :: Opcode
getWindowAttributesOpcode = 3

getInputFocusOpcode :: Opcode
getInputFocusOpcode = 43

createWindowOpcode :: Opcode
createWindowOpcode = 1

mapWindowOpcode :: Opcode
mapWindowOpcode = 8

queryColorsOpcode :: Opcode
queryColorsOpcode = 91

queryPointerOpcode :: Opcode
queryPointerOpcode = 38

grabServerOpcode :: Opcode
grabServerOpcode = 36

ungrabServerOpcode :: Opcode
ungrabServerOpcode = 37

getSelectionOwnerOpcode :: Opcode
getSelectionOwnerOpcode = 23

getGeometryOpcode :: Opcode
getGeometryOpcode = 14

xcMiscOpcode :: Opcode
xcMiscOpcode = 128

xcMiscGetXIDRangeOpcode :: Opcode
xcMiscGetXIDRangeOpcode = 1

--------------------------------------------------------------------------------
-- Atoms

predefinedAtoms :: HashMap AtomID ByteString
predefinedAtoms =
  HM.fromList
    [ (1, "XA_PRIMARY")
    , (2, "XA_SECONDARY")
    , (3, "XA_ARC")
    , (4, "XA_ATOM")
    , (5, "XA_BITMAP")
    , (6, "XA_CARDINAL")
    , (7, "XA_COLORMAP")
    , (8, "XA_CURSOR")
    , (9, "XA_CUT_BUFFER0")
    , (10, "XA_CUT_BUFFER1")
    , (11, "XA_CUT_BUFFER2")
    , (12, "XA_CUT_BUFFER3")
    , (13, "XA_CUT_BUFFER4")
    , (14, "XA_CUT_BUFFER5")
    , (15, "XA_CUT_BUFFER6")
    , (16, "XA_CUT_BUFFER7")
    , (17, "XA_DRAWABLE")
    , (18, "XA_FONT")
    , (19, "XA_INTEGER")
    , (20, "XA_PIXMAP")
    , (21, "XA_POINT")
    , (22, "XA_RECTANGLE")
    , (23, "XA_RESOURCE_MANAGER")
    , (24, "XA_RGB_COLOR_MAP")
    , (25, "XA_RGB_BEST_MAP")
    , (26, "XA_RGB_BLUE_MAP")
    , (27, "XA_RGB_DEFAULT_MAP")
    , (28, "XA_RGB_GRAY_MAP")
    , (29, "XA_RGB_GREEN_MAP")
    , (30, "XA_RGB_RED_MAP")
    , (31, "XA_STRING")
    , (32, "XA_VISUALID")
    , (33, "XA_WINDOW")
    , (34, "XA_WM_COMMAND")
    , (35, "XA_WM_HINTS")
    , (36, "XA_WM_CLIENT_MACHINE")
    , (37, "XA_WM_ICON_NAME")
    , (38, "XA_WM_ICON_SIZE")
    , (39, "XA_WM_NAME")
    , (40, "XA_WM_NORMAL_HINTS")
    , (41, "XA_WM_SIZE_HINTS")
    , (42, "XA_WM_ZOOM_HINTS")
    , (43, "XA_MIN_SPACE")
    , (44, "XA_NORM_SPACE")
    , (45, "XA_MAX_SPACE")
    , (46, "XA_END_SPACE")
    , (47, "XA_SUPERSCRIPT_X")
    , (48, "XA_SUPERSCRIPT_Y")
    , (49, "XA_SUBSCRIPT_X")
    , (50, "XA_SUBSCRIPT_Y")
    , (51, "XA_UNDERLINE_POSITION")
    , (52, "XA_UNDERLINE_THICKNESS")
    , (53, "XA_STRIKEOUT_ASCENT")
    , (54, "XA_STRIKEOUT_DESCENT")
    , (55, "XA_ITALIC_ANGLE")
    , (56, "XA_X_HEIGHT")
    , (57, "XA_QUAD_WIDTH")
    , (58, "XA_WEIGHT")
    , (59, "XA_POINT_SIZE")
    , (60, "XA_RESOLUTION")
    , (61, "XA_COPYRIGHT")
    , (62, "XA_NOTICE")
    , (63, "XA_FONT_NAME")
    , (64, "XA_FAMILY_NAME")
    , (65, "XA_FULL_NAME")
    , (66, "XA_CAP_HEIGHT")
    , (67, "XA_WM_CLASS")
    , (68, "XA_WM_TRANSIENT_FOR")
    ]
