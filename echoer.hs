import Conduit
import Data.Conduit.Network
import System.IO

main :: IO ()
main =
  runTCPServer (serverSettings 5000 "*") $ \appData ->
    runResourceT $
      appSource appData $$
      sinkHandle stdout
