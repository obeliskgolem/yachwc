{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.Environment
import System.Timeout

import qualified Control.Concurrent.Chan.Unagi as UChan

import Text.HTML.Parser
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Control.Exception

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as Map

type URL        = String
type InnerText  = T.Text
type Depth      = Int
type Info       = (InnerText, Depth)
type URLInfo    = (URL, Info)

main :: IO ()
main = do
        globalMap <- newEmptyMVar
        putMVar globalMap (Map.fromList [rootURL])

        toProcess <- newEmptyMVar
        putMVar toProcess [rootURL]

        forM_ [1..maxDepth] $ \x -> do
                l <- takeMVar toProcess
                newList <- mapConcurrently (`testHTTP` globalMap) l
                print $ newList
                putMVar toProcess (join newList)

        m <- takeMVar globalMap
        print $ Map.toList m
        return ()

------- Global Variables and Settings --------
globalHTTP  = managerSetProxy (proxyEnvironment Nothing) defaultManagerSettings
globalHTTPS = managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

websiteURL  = "https://obeliskgolem.github.io/posts/2019-02-28-building-hakyll-site-1.html"

rootURL :: URLInfo
rootURL = (websiteURL, ("首页", 0))

maxDepth = 2

--errorResponse :: Response

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

findAllHrefs :: URLInfo -> [Token] -> [URLInfo]
findAllHrefs url_info@(url, (_, d)) (TagOpen "a" attrs:ContentText t:xs) = if x /= "" then
        ((makeAbsoluteURL url x, (t, d+1))):(findAllHrefs url_info xs)
        else
                findAllHrefs url_info xs
        where x = findHref attrs
findAllHrefs _ [] = []
findAllHrefs url_info (x:xs) = findAllHrefs url_info xs

-------------- Testing          --------
-- testHTTP :: UChan.InChan URLInfo -> URLInfo -> MVar (Map.Map URL Info) -> IO ()
-- testHTTP in_c url_info@(url, (_, d)) gMap = do
--     when (d >= maxDepth) (return ())
--     man <- if ("https://" `isPrefixOf` url) then 
--         newManager globalHTTPSSetting
--         else
--         newManager globalHTTPSetting
--     req <- parseRequest url
--     response <- httpLbs req man
--     m <- takeMVar gMap

--     let parsed_tokens   = parseTokens $ decodeUtf8 $ BSL.toStrict $ responseBody response
--     let page_urls = findAllHrefs rootURL parsed_tokens
--     let new_urls = filter (isNothing . (`Map.lookup` m) . fst) page_urls
--     UChan.writeList2Chan in_c new_urls
--     putMVar gMap (Map.union m (Map.fromList new_urls))
--     return ()
testHTTP :: URLInfo -> MVar (Map.Map URL Info) -> ExceptT HttpExceptionContent IO [URLInfo]
testHTTP url_info@(url, (_, d)) gMap = do
        return ()

        guard (d < maxDepth)
        let setting = if ("https://" `isPrefixOf` url) then globalHTTPS else globalHTTP
        man <- newManager setting
        req <- parseRequest url
        res <- try (httpLbs req man)
        case res of
                Right response  -> do
                        m <- takeMVar gMap
                        let parsed_tokens   = parseTokens $ decodeUtf8 $ BSL.toStrict $ responseBody response
                        let page_urls = findAllHrefs rootURL parsed_tokens
                        let new_urls = filter (isNothing . (`Map.lookup` m) . fst) page_urls

                        putMVar gMap (Map.union m (Map.fromList new_urls))
                        return new_urls
                Left _          -> return []
