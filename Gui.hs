module Main where

import Control.Concurrent
import Control.Monad
import Data.String.Utils
import Graphics.UI.WX
import Graphics.UI.WXCore


import Cod4
import Types

main :: IO ()
main = start hello

hello :: IO ()
hello = do
    win <- frame [text := "Cod4 Server Checker", visible := False]
    ip <- textEntry win []
    port <- textEntry win []
    ok <- button win [text := "Aggiungi"]

    servers <- readServersFromFile "servers.data"

    list <- listCtrl win [columns := [("Ip:Port", AlignLeft, 170),
                                      ("Server Name", AlignLeft, 190),
                                      ("Current Map", AlignLeft, 100),
                                      ("Players", AlignLeft, 50)]
                         ,items := servers
                         ]

    set ok [on click := const $ do
                 i <- get ip text
                 p <- get port text
                 info <- getServerInfo i p
                 itemAppend list info
                 set ip [text := ""]
                 set port [text := ""] ]

    set list [on listEvent := onListEvent win list]

    set win [layout := margin 10 $
             column 4 [hfill $ row 5 [minsize (sz 130 (-1)) $ widget ip,
                                      minsize (sz 60 (-1)) $ widget port,
                                      widget ok],
                                   fill $ boxed "Servers" $ fill $ widget list],
             clientSize := sz 640 480]

    set win [on closing := do
                  servers <- get list items
                  writeServersToFile "servers.data" servers
                  propagateEvent]
    set win [visible := True]

onListEvent :: Frame a -> ListCtrl a -> EventList -> IO ()
onListEvent win list (ListItemActivated index) = do
    l@(ipport:_) <- get list $ item index
    let (ip:port:_) = split ":" ipport
    spots <- checkFreeSpot ip port
    play $ sound "resources/beep-2.wav"
onListEvent _ _ _ = return ()

readServersFromFile :: String -> IO [[String]]
readServersFromFile path = do
    d <- readFile path
    if (null d)
        then return []
        else return $ read d

writeServersToFile :: String -> [[String]] -> IO ()
writeServersToFile path servers = do
    writeFile path (show servers)
