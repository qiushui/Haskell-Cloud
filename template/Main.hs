import Control.Monad
import Control.Exception
import Data.Char
import System.IO
import Network
import Network.BSD
import Network.Socket
import System.Environment

main = do sock <- bracketOnError
                     (getProtocolNumber "tcp" >>= socket AF_INET Stream)
                     sClose
                     (\sock -> do setSocketOption sock ReuseAddr 1
                                  [host,port] <- getArgs
                                  inet <- inet_addr host
                                  bindSocket sock $ SockAddrInet (fromIntegral $ read port) inet
                                  listen sock maxListenQueue
                                  return sock
                     )
          (env,_) <- unzip `liftM` getEnvironment
          let response = "HTTP/1.1 200 OK\r\n\r\nWelcome to Haskell Cloud!\n\n" ++ unlines (zipWith
                           (\var db -> db ++ if var `elem` env then " is installed on $" ++ var ++ "." else " is not installed.")
                           ["OPENSHIFT_MYSQL_DB_URL", "OPENSHIFT_MONGODB_DB_URL", "OPENSHIFT_POSTGRESQL_DB_URL"]
                           ["MySQL", "MongoDB", "PostgreSQL"]) ++
                         "\nIf you have installed a database that is not recognised, restart the Haskell cartridge."
          forever $ do (handle,_,_) <- Network.accept sock
                       read <- liftM (any (null . dropWhile isSpace) . lines) $ hGetContents handle
                       when read $ hPutStr handle response
                       hClose handle
