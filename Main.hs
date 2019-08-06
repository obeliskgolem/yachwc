{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.Environment

import Control.Concurrent.Chan.Unagi

import Text.HTML.Parser
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL

import Data.List
import qualified Data.Map.Strict as Map

type URL        = String
type InnerText  = T.Text
type Depth      = Int
type URLInfo    = (URL, InnerText)

data URLMap     = Map URL Int
        deriving Show

websiteURL :: URL
websiteURL = "https://obeliskgolem.github.io/posts/2019-02-28-building-hakyll-site-1.html"

main :: IO ()
main = do
        (in_c, out_c) <- newChan 
        testHTTP
        return ()

-------------- URL Processing --------------
makeAbsoluteURL :: String -> String -> String
makeAbsoluteURL root rel
        | "http://" `isPrefixOf` rel    = rel
        | "https://" `isPrefixOf` rel   = rel
        | otherwise                     = intercalate "/" (foldl processURLStack [] (T.splitOn (T.pack "/") (T.pack $ root ++ ('/':rel))))

processURLStack :: [String] -> T.Text -> [String]
processURLStack url stack  = case (T.unpack stack) of
        ".."    -> init url
        "."     -> url
        _       -> url ++ [T.unpack stack]

-------------- Processing --------------
findHref :: [Attr] -> String
findHref [] = []
findHref (Attr "href" s:xs) = T.unpack s
findHref (x:xs) = findHref xs

findAllHrefs :: String -> [Token] -> [URLInfo]
findAllHrefs url (TagOpen "a" attrs:ContentText t:xs) = if x /= "" then
        (makeAbsoluteURL url x, t):(findAllHrefs url xs)
        else
                findAllHrefs url xs
        where x = findHref attrs
findAllHrefs _ [] = []
findAllHrefs url (x:xs) = findAllHrefs url xs


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
    let page_urls = findAllHrefs websiteURL parsed_tokens
    writeList2Chan in_c page_urls
    getChanContents out_c >>= print
    --putStrLn $ show $ findAllHrefs parsed_tokens
    return ()