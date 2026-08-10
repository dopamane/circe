module Codec.Circe.Main (circeMain) where

import Codec.Circe.CRC16
import Data.Word
import Options.Applicative

circeMain :: IO ()
circeMain = do
  cli <- circeCLI
  case cli of
    CRC16 cmd -> case cmd of
      CRC16Table p -> print $ showW16 <$> tableCRC16 p

data CirceCLI
  = CRC16 CRC16Cmd

data CRC16Cmd = CRC16Table Word16

circeCLI :: IO CirceCLI
circeCLI = customExecParser prefs' pinfo

prefs' :: ParserPrefs
prefs' = prefs $ showHelpOnError <> showHelpOnEmpty

pinfo :: ParserInfo CirceCLI
pinfo = info (parser <**> simpleVersioner "v0.1.0.0" <**> helper) $ progDesc "CIRCE - CRC"

parser :: Parser CirceCLI
parser = hsubparser $ mconcat
  [ command "16" $ info (CRC16 <$> crc16CmdParser) $ progDesc "CRC16"
  ]

crc16CmdParser :: Parser CRC16Cmd
crc16CmdParser = hsubparser $ mconcat
  [ command "table" $ info (CRC16Table <$> polyArgParser) $ progDesc "Generate CRC16 lookup table with polynomial"
  ]

polyArgParser :: Parser Word16
polyArgParser = argument auto $ metavar "POLY" <> help "polynomial"
