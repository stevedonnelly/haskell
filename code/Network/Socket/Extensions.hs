module Network.Socket.Extensions where
import Control.Concurrent
import Control.Exception
import Data.List as List
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

resolveHostPort :: String -> String -> IO AddrInfo
resolveHostPort = \host port -> do
    addresses <- (lookupHostPort host port)
    (return (List.head addresses))

listeningSocket :: Integral p => p -> IO Socket
listeningSocket = \port -> do
    socket <- inetSocket
    (bind socket (SockAddrInet (toPortNumber port) iNADDR_ANY))
    (listen socket maxListenQueue)
    (return socket)
    
clientSocket :: String -> String -> IO Socket
clientSocket = \host port -> do
    address <- (resolveHostPort host port)
    socket <- inetSocket
    let {handleConnectFailure = \exception -> do 
        (close socket)
        (threadDelay 3000000)
        (clientSocket host port)}
    let handleConnectFailureTyped = handleConnectFailure :: SomeException -> IO Socket
    let {doConnect = do 
        (connect socket (addrAddress address))
        (return socket)}
    (catch doConnect handleConnectFailureTyped)



