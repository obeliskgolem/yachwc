{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.Environment

import Text.HTML.Parser
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL

type URL        = String
type InnerText  = T.Text
type Depth      = Int
type URLInfo    = (InnerText, Depth)

data URLMap     = Map URL URLInfo
        deriving Show

websiteURL :: URL
websiteURL = "https://obeliskgolem.github.io/"

websiteInfo :: URLInfo
websiteInfo = ("首页", 0)

main :: IO ()
main = do
        putStrLn "hello world"
--        processMap websiteURL websiteInfo Map.empty
        return ()

-------------- Processing --------------


-------------- Testing          --------
testHTTP :: IO ()
testHTTP = do
    let settings = managerSetProxy
            (proxyEnvironment Nothing)
            tlsManagerSettings
    man <- newManager settings
    req <- parseRequest websiteURL
    response <- httpLbs req man 
    let parsed_tokens   = parseTokens $ decodeUtf8 $ BSL.toStrict $ responseBody response
    print $ parsed_tokens
    return ()