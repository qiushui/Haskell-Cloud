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
          forever $ do (handle,_,_) <- Network.accept sock
                       read <- liftM (any (null . dropWhile isSpace) . lines) $ hGetContents handle
                       when read $ hPutStr handle "HTTP/1.1 200 OK\r\n\r\nWelcome to Haskell Cloud"
                       hClose handle
