module Server where

import System.IO
import System.Environment (lookupEnv)
import Network
import Network.Socket hiding (accept)

handleRequest :: Handle -> IO ()
handleRequest handle = do
    content <- fmap lines (hGetContents handle)
    putStrLn $ head content
    hPutStr handle "HTTP/1.1 200 OK\r\n\r\nHello world."
    return ()

mainLoop :: Socket -> IO ()
mainLoop socket = do
    (handle, hostname, port) <- accept socket
    putStrLn $ "Request from " ++ hostname
    handleRequest handle
    hClose handle
    mainLoop socket

start :: IO ()
start = do
    maybePort <- lookupEnv "SRV_PORT"
    let port = maybe 9000 (\x -> read x :: Int) maybePort
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    putStrLn $ "Start server on: " ++ (show port)
    bind sock (SockAddrInet (fromIntegral port) iNADDR_ANY)
    listen sock 4
    mainLoop sock
