import Conduit
import Data.Conduit.Network

main :: IO ()
main =
  runTCPServer (serverSettings 5000 "*") $ \appData ->
    runResourceT $
      appSource appData $$
      sinkFile "data"
