module Codec.Circe.Main (circeMain) where

import Codec.Circe.CRC16
import Codec.Circe.CRC32
import Codec.Circe.Cfg
import Codec.Circe.Pretty
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.Word
import Options.Applicative

-- | main app
circeMain :: IO ()
circeMain = do
  cli <- circeCLI "v0.1.0.0"
  case cli of
    CRC16Table p -> putStrLn $ unlines [unwords c | c <- chunks 8 $ w16 <$> crc16Table p]
    CRC16 cfg (StringInput s) -> putStrLn $ w16 $ crc16 cfg $ BSC.pack s
    CRC16 cfg (FileInput f) -> putStrLn . w16 . crc16 cfg =<< BS.readFile f
    CRC16 cfg StdInput -> putStrLn . w16 . crc16 cfg =<< BSC.getContents
    CRC32Table p -> putStrLn $ unlines [unwords c | c <- chunks 8 $ w32 <$> crc32Table p]

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

data CirceCLI
  = -- | poly, init, refin, refout, finxor, input
    CRC16 CRC16Cfg Input
  | -- | poly
    CRC16Table Word16
  | CRC32Table Word32

data Input
  = StringInput String
  | FileInput String
  | StdInput

-- | run CLI, specify version string
circeCLI :: String -> IO CirceCLI
circeCLI = customExecParser prefs' . pinfo

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: String -> ParserInfo CirceCLI
pinfo vStr = info (parser <**> simpleVersioner vStr <**> helper) $ progDesc "CIRCE - CRC"

parser :: Parser CirceCLI
parser = hsubparser $ mconcat
  [ command "16" $ info crc16Parser $ progDesc "CRC16"
  , command "32" $ info crc32Parser $ progDesc "CRC32"
  ]

crc16Parser :: Parser CirceCLI
crc16Parser = asum
  [ CRC16Table <$> tableOpt 16
  , CRC16 <$> crc16CfgParser <*> inputOpt
  ]

crc32Parser :: Parser CirceCLI
crc32Parser = asum
  [ CRC32Table <$> tableOpt 32
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

inputOpt :: Parser Input
inputOpt = asum
  [ StringInput <$> stringOpt
  , FileInput <$> fileOpt
  , flag' StdInput $ long "stdin" <> help "Stream from stdin"
  ]

stringOpt :: Parser String
stringOpt = strOption $ short 's' <> long "string" <> metavar "INPUT" <> help "Input string"

fileOpt :: Parser String
fileOpt = strOption $
  short 'f' <> long "file" <> metavar "PATH" <> completer (bashCompleter "file")
    <> help "Input binary file"
