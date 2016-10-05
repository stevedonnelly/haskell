module Network.Socket.Extensions where
import Network.Socket

extended_hints = defaultHints {addrFlags = [AI_ADDRCONFIG], addrSocketType = Stream}

inetSocket :: IO Socket
inetSocket = do 
    the_socket <- (socket AF_INET Stream defaultProtocol)
    (setSocketOption the_socket ReuseAddr 1)
    (return the_socket)

toPortNumber :: Integral a => a -> PortNumber
toPortNumber = \port -> (fromInteger (toInteger port))

lookupHostPort :: String -> String -> IO [AddrInfo]
lookupHostPort = \host port -> (getAddrInfo (Just extended_hints) (Just host) (Just port))

lookupHost :: String -> IO [AddrInfo]
lookupHost = \host -> (getAddrInfo (Just extended_hints) (Just host) Nothing)

listeningSocket :: Integral p => p -> IO Socket
listeningSocket = \port -> do
    socket <- inetSocket
    (bind socket (SockAddrInet (toPortNumber port) iNADDR_ANY))
    (listen socket maxListenQueue)
    (return socket)
    

