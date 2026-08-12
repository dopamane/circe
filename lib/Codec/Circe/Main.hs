module Codec.Circe.Main (circeMain) where

import Codec.Circe.CRC16
import Codec.Circe.CRC32
import Codec.Circe.Cfg
import Codec.Circe.Pretty
import qualified Data.ByteString.Lazy as BS
import Data.Version
import Data.Word
import Options.Applicative
import Paths_circe

-- | main app
circeMain :: IO ()
circeMain = do
  cli <- circeCLI $ showVersion version
  case cli of
    CRC16Table p -> putStrLn $ table $ w16 <$> crc16Table p
    CRC32Table p -> putStrLn $ table $ w32 <$> crc32Table p
    CRC16 cfg (Just f) -> putStrLn . w16 . crc16 cfg =<< BS.readFile f
    CRC16 cfg Nothing  -> putStrLn . w16 . crc16 cfg =<< BS.getContents
    CRC32 cfg (Just f) -> putStrLn . w32 . crc32 cfg =<< BS.readFile f
    CRC32 cfg Nothing  -> putStrLn . w32 . crc32 cfg =<< BS.getContents

data CirceCLI
  = CRC16 CRC16Cfg (Maybe String)
  | CRC32 CRC32Cfg (Maybe String)
  | -- | poly
    CRC16Table Word16
  | CRC32Table Word32

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
  [ command "16" $ info crc16Parser $ progDesc "CRC16"
  , command "32" $ info crc32Parser $ progDesc "CRC32"
  ]

crc16Parser :: Parser CirceCLI
crc16Parser = asum
  [ CRC16Table <$> tableOpt 16
  , CRC16 <$> crc16CfgParser <*> optional fileArg
  ]

crc32Parser :: Parser CirceCLI
crc32Parser = asum
  [ CRC32Table <$> tableOpt 32
  , CRC32 <$> crc32CfgParser <*> optional fileArg
  ]

tableOpt :: (Num a, Read a) => Int -> Parser a
tableOpt n = option auto $ mconcat
  [ short 't' <> long "table" <> metavar "POLY"
  , help $ "Generate CRC" <> show n <> " lookup table with polynomial"
  ]

crc16CfgParser :: Parser CRC16Cfg
crc16CfgParser = asum
  [ flag' crc16CCITZero $ long "ccit-zero"
  , flag' crc16Modbus $ long "modbus"
  , CRCCfg
      <$> polyOpt
      <*> initOpt
      <*> refinSwitch
      <*> refoutSwitch
      <*> finXorOpt
  ]

crc32CfgParser :: Parser CRC32Cfg
crc32CfgParser = asum
  [ CRCCfg
      <$> option auto (short 'p' <> long "poly" <> metavar "WORD32" <> help "polynomial")
      <*> option auto (short 'i' <> long "init" <> metavar "WORD32" <> help "initial value")
      <*> refinSwitch
      <*> refoutSwitch
      <*> option auto (short 'x' <> long "xor" <> metavar "WORD32" <> help "final xor")
  , flag crc32IEEE crc32IEEE $ long "ieee"
      <> help ("Default CRC32 config "
                 <> "POLY=0x4C11DB7 INIT=0xFFFFFFFF REFL-IN REFL-OUT FIN-XOR=0xFFFFFFFF")
  ]

polyOpt :: Parser Word16
polyOpt = option auto $
  short 'p' <> long "poly" <> metavar "WORD16" <> help "polynomial"
    <> value 0x1021 <> showDefaultWith w16

initOpt :: Parser Word16
initOpt = option auto $
  short 'i' <> long "init" <> metavar "WORD16" <> help "inital value"
    <> value 0 <> showDefaultWith w16

refinSwitch :: Parser Bool
refinSwitch = switch $ long "ri" <> long "refin" <> help "reflect input"

refoutSwitch :: Parser Bool
refoutSwitch = switch $ long "ro" <> long "refout" <> help "reflect output"

finXorOpt :: Parser Word16
finXorOpt = option auto $
  short 'x' <> long "xor" <> metavar "WORD16"
    <> value 0 <> showDefaultWith w16 <> help "final xor"

fileArg :: Parser String
fileArg = strArgument $
  metavar "FILE" <> completer (bashCompleter "file")
    <> help "Optional binary input file otherwise stream STDIN."
