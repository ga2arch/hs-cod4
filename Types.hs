module Types where

data Server = Server { serverIp :: String
                     , serverPort :: Int
                     }

type ServerInfo = ([(String, String)], [(Int, Int, String)])
