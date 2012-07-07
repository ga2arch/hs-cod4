{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cod4 where

import Data.Maybe (fromJust)
import System.Process (runCommand, ProcessHandle)
import Control.Concurrent (threadDelay)
import Data.String.Utils (split, replace)
import System.Info (os)
import System.IO (hFlush, stdout)
import Control.Monad
import System.Timeout
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as C

import Types

duckIp = "109.168.115.247"
duckPort = 28965

{--main :: IO ()
main = do
    (s, addr) <- prepareSocket duckIp duckPort
    mainLoop s addr 0
    return ()--}

getServerInfo :: String -> String -> IO [String]
getServerInfo ip port = do
    (sock, addr) <- prepareSocket ip (read port :: Int)
    rsp <- queryServer sock addr
    sClose sock

    let infos@(info, players) = parseRsp rsp
    let name = fromJust $ lookup "sv_hostname" info
    let privatecls = read (fromJust $ lookup "sv_privateClients" info) :: Int
    let maxcls = read (fromJust $ lookup "sv_maxclients" info) :: Int
    let spots = getSpots infos
    let currentmap = fromJust $ lookup "mapname" info
    return $ [ip ++ ":" ++ port,
              name,
              currentmap,
              (show $ maxcls - spots - privatecls) ++ "/" ++ (show maxcls)]

checkFreeSpot :: String -> String -> IO (Int)
checkFreeSpot ip port = do
    (sock, addr) <- prepareSocket ip (read port :: Int)
    rsp <- queryServer sock addr
    sClose sock

    let infos = parseRsp rsp
    let spots = getSpots infos
    if spots > 0
        then return spots
        else checkFreeSpot ip port

prepareSocket :: Integral a => String -> a -> IO (Socket, SockAddr)
prepareSocket ip port = do
    s <- socket AF_INET Datagram defaultProtocol
    a <- inet_addr ip
    let addr = SockAddrInet (fromIntegral port :: PortNumber) a
    return (s, addr)

queryServer :: Socket -> SockAddr -> IO C.ByteString
queryServer s addr = do
    sent <- sendTo s "\255\255\255\255 getstatus" addr
    res <- timeout 2 $ recv s 4096
    case res of
        Just x  -> return x
        Nothing -> queryServer s addr

getSpots :: ServerInfo -> Int
getSpots (info, players) = aSpots - (length players)
  where
    tSpots = read (fromJust $ lookup "sv_maxclients" info) :: Int
    pSpots = read (fromJust $ lookup "sv_privateClients" info) :: Int
    aSpots = tSpots - pSpots

parseRsp :: C.ByteString -> ServerInfo
parseRsp rsp = (info, players)
  where
    (_:rawInfo:rawPlayers) = split "\n" $ C.unpack rsp
    info = tuplize . tail . split "\\" $ rawInfo
    players = map (parser . split " ") $ init rawPlayers
    parser (points:ping:nick:_) =
        (read points :: Int, read ping :: Int, replace "\"" "" nick)

tuplize :: [a] -> [(a, a)]
tuplize [] = []
tuplize (x:y:xs) = (x, y) : tuplize xs
