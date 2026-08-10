module Codec.Circe.Main (circeMain) where

import Codec.Circe.CRC16
import Codec.Circe.Pretty
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Vector.Storable as V
import Data.Word
import Options.Applicative

-- | main app
circeMain :: IO ()
circeMain = do
  cli <- circeCLI "v0.1.0.0"
  case cli of
    CRC16Table p -> putStrLn $ unlines [unwords c | c <- chunks 8 $ w16 <$> crc16Table p]
    CRC16 p i _ _ _ s ->
      let t = V.fromList $ crc16Table p
      in putStrLn $ w16 $ crc16 t i $ BSC.pack s

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
    let (ys, zs) = splitAt n xs
    in  ys : chunks n zs

data CirceCLI
  = -- | poly, init, refin, refout, finxor, string
    CRC16 Word16 Word16 Bool Bool Word16 String
  | -- | poly
    CRC16Table Word16

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
  ]

crc16Parser :: Parser CirceCLI
crc16Parser = asum
  [ fmap CRC16Table
      $ option auto
      $ mconcat
        [ short 't' <> long "table" <> metavar "POLY"
        , help "Generate CRC16 lookup table with polynomial"
        ]
  , CRC16
      <$> polyOpt
      <*> initOpt
      <*> refinSwitch
      <*> refoutSwitch
      <*> finXorOpt
      <*> stringOpt
  ]

polyOpt :: Parser Word16
polyOpt = option auto $ short 'p' <> long "poly" <> metavar "WORD16" <> help "polynomial"

initOpt :: Parser Word16
initOpt = option auto $ short 'i' <> long "init" <> metavar "WORD16" <> help "inital value"

refinSwitch :: Parser Bool
refinSwitch = switch $ long "ri" <> long "refin" <> help "reflect input"

refoutSwitch :: Parser Bool
refoutSwitch = switch $ long "ro" <> long "refout" <> help "reflect output"

finXorOpt :: Parser Word16
finXorOpt = option auto $
  short 'x' <> long "xor" <> metavar "WORD16" <> value 0 <> showDefault <> help "final xor"

stringOpt :: Parser String
stringOpt = strOption $ short 's' <> long "string" <> metavar "INPUT" <> help "Input string"
