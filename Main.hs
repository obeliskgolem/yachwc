{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.Environment
import System.Timeout

import qualified Control.Concurrent.Chan.Unagi as UChan

import Text.HTML.Parser
import Data.Text.Encoding
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
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
        [websiteURL, maxDepth] <- getArgs
        let d = read maxDepth :: Int
        let rootURL = (websiteURL, ("首页", 0))

        globalMap <- newEmptyMVar
        putMVar globalMap (Map.fromList [rootURL])

        toProcess <- newEmptyMVar
        putMVar toProcess [rootURL]

        forM_ [1..d] $ \x -> do
                l <- takeMVar toProcess
                let url_list = filter ((< d) . snd . snd) l
                newList <- mapConcurrently (`testHTTP` globalMap) url_list
                putMVar toProcess (join newList)

        m <- takeMVar globalMap
        mapM_ (`printURLInfo` d) (Map.toList m)

--------------  Print URL Info ---------------
printURLInfo :: URLInfo -> Int -> IO ()
printURLInfo (url, (t, d)) maxD = if (d == maxD) then do { putStr $ url ++ "\t"; T.IO.putStrLn t;} else return ()

------- Global Variables and Settings --------
globalHTTP  = managerSetProxy (proxyEnvironment Nothing) defaultManagerSettings
globalHTTPS = managerSetProxy (proxyEnvironment Nothing) tlsManagerSettings

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
findAllHrefs url_info@(url, (str, d)) (TagOpen "a" attrs:ContentText t:xs) = if x /= "" then
        ((makeAbsoluteURL url x, (str `T.append` ('-' `T.cons` t), d+1))):(findAllHrefs url_info xs)
        else
                findAllHrefs url_info xs
        where x = findHref attrs
findAllHrefs _ [] = []
findAllHrefs url_info (x:xs) = findAllHrefs url_info xs

-------------- Testing          --------
testHTTP :: URLInfo -> MVar (Map.Map URL Info) -> IO [URLInfo]
testHTTP url_info@(url, (_, _)) gMap = do
        catch (processResponse) (\(HttpExceptionRequest _ _) -> return [])
        where
                processResponse = do
                        let setting = if ("https://" `isPrefixOf` url) then globalHTTPS else globalHTTP
                        man <- newManager setting
                        req <- parseRequest url
                        response <- httpLbs req man
                        m <- takeMVar gMap
                        let parsed_tokens   = parseTokens $ decodeUtf8 $ BSL.toStrict $ responseBody response
                        let page_urls = findAllHrefs url_info parsed_tokens
                        let new_urls = filter (isNothing . (`Map.lookup` m) . fst) page_urls

                        putMVar gMap (Map.union m (Map.fromList new_urls))
                        return new_urls
