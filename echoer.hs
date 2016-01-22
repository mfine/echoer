import           Conduit
import qualified Data.Conduit.Combinators as C
import           Data.Conduit.Network
import           Data.Time
import           Options.Applicative

data Conf = Conf
  { confFile  :: String
  , confPort  :: Int
  , confBytes :: Maybe Int
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

bytes :: Parser Int
bytes =
  option auto
    $  long    "bytes"
    <> short   'n'
    <> metavar "BYTES"
    <> help    "Number of bytes to listen to"

parseConf :: Parser Conf
parseConf = Conf <$> file <*> port <*> optional bytes

parser :: Parser a -> ParserInfo a
parser parse = info ( helper <*> parse ) fullDesc

main :: IO ()
main = do
  c <- execParser $ parser parseConf
  putStrLn $ "Starting on :" ++ (show $ confPort c)
  runTCPServer (serverSettings (confPort c) "*") $ \appData -> do
    t <- getCurrentTime
    putStrLn $ "B " ++ name c t
    runResourceT $
      appSource appData $$
      maybe id C.takeExactlyE (confBytes c) $
        sinkFile $ name c t
    t' <- getCurrentTime
    putStrLn $ "E " ++ name c t ++ " " ++ timediff t' t where
      name c t = confFile c ++ "-" ++ timestamp t ++ ".dat"
      timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S"
      timediff t = show . diffUTCTime t

