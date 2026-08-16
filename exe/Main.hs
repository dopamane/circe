module Main (main) where

import Codec.Circe
import Codec.Circe.Internal
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector.Storable as V
import Data.Version
import Data.Word
import Options.Applicative
import Paths_circe

main :: IO ()
main = do
  cli <- circeCLI $ showVersion version
  case cli of
    CRC8Table  p -> display w8  $ crc8Table  p
    CRC16Table p -> display w16 $ crc16Table p
    CRC32Table p -> display w32 $ crc32Table p
    CRC64Table p -> display w64 $ crc64Table p
    CRC8  cfg fM -> stream fM $ w8  . crc8  cfg
    CRC16 cfg fM -> stream fM $ w16 . crc16 cfg
    CRC32 cfg fM -> stream fM $ w32 . crc32 cfg
    CRC64 cfg fM -> stream fM $ w64 . crc64 cfg

display :: V.Storable a => (a -> String) -> V.Vector a -> IO ()
display f = putStrLn . table . map f . V.toList

stream :: Maybe String -> (ByteString -> String) -> IO ()
stream Nothing     f = putStrLn . f =<< BS.getContents
stream (Just file) f = putStrLn . f =<< BS.readFile file

data CirceCLI
  = CRC8  CRC8Cfg  (Maybe String)
  | CRC16 CRC16Cfg (Maybe String)
  | CRC32 CRC32Cfg (Maybe String)
  | CRC64 CRC64Cfg (Maybe String)
  | CRC8Table  Word8
  | CRC16Table Word16
  | CRC32Table Word32
  | CRC64Table Word64

-- | run CLI, specify version string
circeCLI :: String -> IO CirceCLI
circeCLI = customExecParser prefs' . pinfo

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CirceCLI
pinfo vStr = info (parser <**> simpleVersioner vStr <**> helper) $
  progDesc "CIRCE - cyclic redundancy check"

parser :: Parser CirceCLI
parser = hsubparser $ mconcat
  [ command "8"  $ info crc8Parser  $ progDesc "CRC8"
  , command "16" $ info crc16Parser $ progDesc "CRC16"
  , command "32" $ info crc32Parser $ progDesc "CRC32"
  , command "64" $ info crc64Parser $ progDesc "CRC64"
  ]
  where
    crc8Parser  = liftA2 CRC8  crc8CfgParser  (optional fileArg) <|> CRC8Table  `fmap` tableOpt 8
    crc16Parser = liftA2 CRC16 crc16CfgParser (optional fileArg) <|> CRC16Table `fmap` tableOpt 16
    crc32Parser = liftA2 CRC32 crc32CfgParser (optional fileArg) <|> CRC32Table `fmap` tableOpt 32
    crc64Parser = liftA2 CRC64 crc64CfgParser (optional fileArg) <|> CRC64Table `fmap` tableOpt 64

tableOpt :: (Num a, Read a) => Int -> Parser a
tableOpt n = option auto $ mconcat
  [ short 't' <> long "table" <> metavar "POLY"
  , help $ "Generate CRC" <> show n <> " lookup table with polynomial"
  ]

crc8CfgParser :: Parser CRC8Cfg
crc8CfgParser = asum
  [ flag' crc8Cfg $ long "crc8" <> help ("DEFAULT " ++ showCRC8Cfg crc8Cfg)
  , flag' crc88H2F $ long "8h2f" <> help (showCRC8Cfg crc88H2F)
  , flag' crc8CDMA2k $ long "cdma2k" <> help (showCRC8Cfg crc8CDMA2k)
  , flag' crc8DARC $ long "darc" <> help (showCRC8Cfg crc8DARC)
  , flag' crc8DVBS2 $ long "dvbs2" <> help (showCRC8Cfg crc8DVBS2)
  , flag' crc8EBU $ long "ebu" <> help (showCRC8Cfg crc8EBU)
  , flag' crc8ICode $ long "icode" <> help (showCRC8Cfg crc8ICode)
  , flag' crc8ITU $ long "itu" <> help (showCRC8Cfg crc8ITU)
  , flag' crc8Maxim $ long "maxim" <> help (showCRC8Cfg crc8Maxim)
  , flag' crc8ROHC $ long "rohc" <> help (showCRC8Cfg crc8ROHC)
  , flag' crc8WCDMA $ long "wcdma" <> help (showCRC8Cfg crc8WCDMA)
  , crcCfgParser
  , pure crc8Cfg
  ]

crc16CfgParser :: Parser CRC16Cfg
crc16CfgParser = asum
  [ flag' crc16CCITZero $ long "ccit-zero" <> help ("DEFAULT " ++ showCRC16Cfg crc16CCITZero)
  , flag' crc16Arc $ long "arc" <> help (showCRC16Cfg crc16Arc)
  , flag' crc16AugCCITT $ long "aug-ccit" <> help (showCRC16Cfg crc16AugCCITT)
  , flag' crc16Buypass $ long "buypass" <> help (showCRC16Cfg crc16Buypass)
  , flag' crc16CCITTFalse $ long "ccitt-false" <> help (showCRC16Cfg crc16CCITTFalse)
  , flag' crc16CDMA2k $ long "cdma2k" <> help (showCRC16Cfg crc16CDMA2k)
  , flag' crc16DDS110 $ long "dds110" <> help (showCRC16Cfg crc16DDS110)
  , flag' crc16DECTR $ long "dectr" <> help (showCRC16Cfg crc16DECTR)
  , flag' crc16DECTX $ long "dectx" <> help (showCRC16Cfg crc16DECTX)
  , flag' crc16DNP $ long "dnp" <> help (showCRC16Cfg crc16DNP)
  , flag' crc16EN13757 $ long "en13757" <> help (showCRC16Cfg crc16EN13757)
  , flag' crc16Genibus $ long "genibus" <> help (showCRC16Cfg crc16Genibus)
  , flag' crc16Maxim $ long "maxim" <> help (showCRC16Cfg crc16Maxim)
  , flag' crc16MCRF4XX $ long "mcrf4xx" <> help (showCRC16Cfg crc16MCRF4XX)
  , flag' crc16Riello $ long "riello" <> help (showCRC16Cfg crc16Riello)
  , flag' crc16T10DIF $ long "t10dif" <> help (showCRC16Cfg crc16T10DIF)
  , flag' crc16Teledisk $ long "teledisk" <> help (showCRC16Cfg crc16Teledisk)
  , flag' crc16TMS37157 $ long "tms37157" <> help (showCRC16Cfg crc16TMS37157)
  , flag' crc16USB $ long "usb" <> help (showCRC16Cfg crc16USB)
  , flag' crc16CRCA $ long "crca" <> help (showCRC16Cfg crc16CRCA)
  , flag' crc16Kermit $ long "kermit" <> help (showCRC16Cfg crc16Kermit)
  , flag' crc16Modbus $ long "modbus" <> help (showCRC16Cfg crc16Modbus)
  , flag' crc16X25 $ long "x25" <> help (showCRC16Cfg crc16X25)
  , flag' crc16XModem $ long "xmodem" <> help (showCRC16Cfg crc16XModem)
  , crcCfgParser
  , pure crc16CCITZero
  ]

crc32CfgParser :: Parser CRC32Cfg
crc32CfgParser = asum
  [ flag' crc32IEEE $ long "ieee" <> help ("DEFAULT " ++ showCRC32Cfg crc32IEEE)
  , flag' crc32BZIP2 $ long "bzip2" <> help (showCRC32Cfg crc32BZIP2)
  , flag' crc32MPEG2 $ long "mpeg2" <> help (showCRC32Cfg crc32MPEG2)
  , flag' crc32POSIX $ long "posix" <> help (showCRC32Cfg crc32POSIX)
  , crcCfgParser
  , pure crc32IEEE
  ]

crc64CfgParser :: Parser CRC64Cfg
crc64CfgParser = asum
  [ flag' crc64ECMA182 $ long "ecma-182" <> help ("DEFAULT " ++ showCRC64Cfg crc64ECMA182)
  , flag' crc64GoISO $ long "go-iso" <> help (showCRC64Cfg crc64GoISO)
  , flag' crc64WE $ long "we" <> help (showCRC64Cfg crc64WE)
  , flag' crc64XZ $ long "xz" <> help (showCRC64Cfg crc64XZ)
  , crcCfgParser
  , pure crc64ECMA182
  ]

crcCfgParser :: Read a => Parser (CRCCfg a)
crcCfgParser =
  CRCCfg
    <$> option auto (short 'p' <> long "poly" <> help "polynomial")
    <*> option auto (short 'i' <> long "init" <> help "initial value")
    <*> switch (long "ri" <> long "refin" <> help "reflect input")
    <*> switch (long "ro" <> long "refout" <> help "reflect output")
    <*> option auto (short 'x' <> long "xor" <> help "final xor")

fileArg :: Parser String
fileArg = strArgument $ metavar "FILE" <> completer (bashCompleter "file")
  <> help "Optional binary input file otherwise stream STDIN."
