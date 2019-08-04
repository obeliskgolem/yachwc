{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.Environment

import Text.HTML.Parser
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL

import Data.List

type URL        = String
type InnerText  = T.Text
type Depth      = Int
type URLInfo    = (URL, InnerText)

data URLMap     = Map URL Int
        deriving Show

websiteURL :: URL
-- websiteURL = "https://obeliskgolem.github.io/"
websiteURL = "https://obeliskgolem.github.io/posts/2019-02-28-building-hakyll-site-1.html"

--websiteInfo :: URLInfo
--websiteInfo = ("首页", 0)

main :: IO ()
main = do
        putStrLn "hello world"
--        processMap websiteURL websiteInfo Map.empty
        return ()

-------------- URL Processing --------------
makeAbsoluteURL :: String -> String -> String
makeAbsoluteURL root rel 
        | "http://" `isPrefixOf` rel    = rel
        | "https://" `isPrefixOf` rel   = rel
        | otherwise                     = root ++ ('/':rel)

data URLAction = None | Pop | Push String

generateActions :: String -> URLAction
generateActions ".."   = Pop
generateActions "."    = None
generateActions ""     = None
generateActions s      = Push s

makeValidURL :: [String] -> [String]
makeValidURL (x:_:"..":xs) = makeValidURL (x:xs)
makeValidURL (".":xs) = makeValidURL xs
makeValidURL ("":xs) = makeValidURL xs
makeValidURL (x:xs) = x:(makeValidURL xs)
makeValidURL [] = []

-------------- Processing --------------
findHref :: [Attr] -> String
findHref [] = []
findHref (Attr "href" s:xs) = T.unpack s
findHref (x:xs) = findHref xs

findAllHrefs :: [Token] -> [URLInfo]
findAllHrefs (TagOpen "a" attrs:ContentText t:xs) = (findHref attrs, t):(findAllHrefs xs)
findAllHrefs [] = []
findAllHrefs (x:xs) = findAllHrefs xs


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
    print $ findAllHrefs parsed_tokens
    return ()