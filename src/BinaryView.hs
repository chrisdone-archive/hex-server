{-# OPTIONS_GHC -Wall #-}

-- | Dump binary a bit like a hex editor, but in decimal on the left.
--
-- > dumpBinary 10 (S8.pack [toEnum 0 .. toEnum 254])
-- 000 001 002 003 004 005 006 007 008 009    . . . . . . . . . .
-- 010 011 012 013 014 015 016 017 018 019    . . . . . . . . . .
-- 020 021 022 023 024 025 026 027 028 029    . . . . . . . . . .
-- 030 031 032 033 034 035 036 037 038 039    . .   ! " # $ % & '
-- 040 041 042 043 044 045 046 047 048 049    ( ) * + , - . / 0 1
-- 050 051 052 053 054 055 056 057 058 059    2 3 4 5 6 7 8 9 : ;
-- 060 061 062 063 064 065 066 067 068 069    < = > ? @ A B C D E
-- 070 071 072 073 074 075 076 077 078 079    F G H I J K L M N O
-- 080 081 082 083 084 085 086 087 088 089    P Q R S T U V W X Y
-- 090 091 092 093 094 095 096 097 098 099    Z [ \ ] ^ _ ` a b c
-- 100 101 102 103 104 105 106 107 108 109    d e f g h i j k l m
-- 110 111 112 113 114 115 116 117 118 119    n o p q r s t u v w
-- 120 121 122 123 124 125 126 127 128 129    x y z { | } ~ . . .
-- 130 131 132 133 134 135 136 137 138 139    . . . . . . . . . .
-- 140 141 142 143 144 145 146 147 148 149    . . . . . . . . . .
-- 150 151 152 153 154 155 156 157 158 159    . . . . . . . . . .
-- 160 161 162 163 164 165 166 167 168 169      ¡ ¢ £ ¤ ¥ ¦ § ¨ ©
-- 170 171 172 173 174 175 176 177 178 179    ª « ¬ . ® ¯ ° ± ² ³
-- 180 181 182 183 184 185 186 187 188 189    ´ µ ¶ · ¸ ¹ º » ¼ ½
-- 190 191 192 193 194 195 196 197 198 199    ¾ ¿ À Á Â Ã Ä Å Æ Ç
-- 200 201 202 203 204 205 206 207 208 209    È É Ê Ë Ì Í Î Ï Ð Ñ
-- 210 211 212 213 214 215 216 217 218 219    Ò Ó Ô Õ Ö × Ø Ù Ú Û
-- 220 221 222 223 224 225 226 227 228 229    Ü Ý Þ ß à á â ã ä å
-- 230 231 232 233 234 235 236 237 238 239    æ ç è é ê ë ì í î ï
-- 240 241 242 243 244 245 246 247 248 249    ð ñ ò ó ô õ ö ÷ ø ù
-- 250 251 252 253 254                        ú û ü ý þ

module BinaryView where

import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Char
import           Data.Monoid
import           Text.Printf

dumpBinary :: Int -> ByteString -> IO ()
dumpBinary columns bytes = putStrLn (binaryView columns bytes)

binaryView :: Int -> ByteString -> String
binaryView 0 _ = ""
binaryView columns bytes =
    if S.null bytes
       then mempty
       else pad (columns * 4) (asDec thisLine) <> "   " <> asPrintable thisLine <> "\n" <>
            if S.null otherLines
                then mempty
                else binaryView columns otherLines
  where
    (thisLine,otherLines) = S8.splitAt columns bytes
    asDec = unwords . map (printf "%03d") . S.unpack
    asPrintable = unwords . map printChar . S.unpack
    pad n = printf ("%-" ++ show n ++ "s")
    printChar c | isPrint (toEnum (fromIntegral c)) = printf "%c" c
                | otherwise = "."
