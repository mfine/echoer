import Conduit
import Data.Conduit.Network
import Data.Time
import Options.Applicative

data Conf = Conf
  { confFile :: String
  , confPort :: Int
  } deriving ( Eq, Show )

file :: Parser String
file =
  strOption
    $  long    "file"
    <> short   'f'
    <> metavar "FILE"
    <> value   "data"
    <> help    "File prefix to write"

port :: Parser Int
port =
  option auto
    $  long    "port"
    <> short   'p'
    <> metavar "PORT"
    <> value   5000
    <> help    "Port to listen on"

parseConf :: Parser Conf
parseConf = Conf <$> file <*> port

parser :: Parser a -> ParserInfo a
parser parse = info ( helper <*> parse ) fullDesc

main :: IO ()
main = do
  c <- execParser $ parser parseConf
  t <- getCurrentTime
  runTCPServer (serverSettings (confPort c) "*") $ \appData ->
    runResourceT $
      appSource appData $$
      sinkFile (confFile c ++ "-" ++ timestamp t) where
        timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S"

