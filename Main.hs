{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import System.Environment

import Text.HTML.Parser

import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Text.Encoding as Enc
import qualified Data.ByteString.Lazy as BSL

import qualified Text.Show.Unicode as TSU

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified Control.Monad as Monad

type URL        = String
type InnerText  = T.Text
type Depth      = Int
type URLInfo    = (InnerText, Depth)

data URLMap     = Map URL URLInfo
        deriving Show

data URLPair = URLPair (URL, InnerText)
        deriving Show

websiteURL :: URL
--websiteURL = "http://www.qunzh.com/index.html"
websiteURL = "https://obeliskgolem.github.io/"

websiteInfo :: URLInfo
websiteInfo = ("首页", 0)

main :: IO ()
main = do
        putStrLn "hello world"
        processMap websiteURL websiteInfo Map.empty
        return ()
-- --    [rootURL, maxDepth] <- getArgs

--     request <- parseRequest websiteURL
--     manager <- newManager tlsManagerSettings
--     --manager <- newManager defaultManagerSettings

--     response <- httpLbs request manager 

--     let parsed_html     = Enc.decodeUtf8 $ BSL.toStrict $ responseBody response
--     let parsed_tokens   = parseTokens parsed_html
    
--     --TSU.uprint $ goThrough (parentURL websiteURL) parsed_tokens


-------------- Processing --------------

-- Generate a list of URLs from given root URL, no duplicates
processMap :: URL -> URLInfo -> URLMap -> IO URLMap
processMap url info parsedURL   = case Map.lookup url parsedURL of
                                        Just x  -> return Map.empty
                                        Nothing -> Monad.liftM Map.fromList (goThroughURL url info)

goThroughURL :: URL -> URLInfo -> IO [(URL, URLInfo)]
goThroughURL url info      = do
    request <- parseRequest url
    manager <- newManager tlsManagerSettings

    manager <- case url of
        ('h':'t':'t':'p':':':xs)        -> newManager tlsManagerSettings
        _                               -> newManager defaultManagerSettings

    response <- httpLbs request manager 

    let parsed_html     = Enc.decodeUtf8 $ BSL.toStrict $ responseBody response
    let parsed_tokens   = parseTokens parsed_html

    return $ expandURL url info parsed_tokens

expandURL :: URL -> URLInfo -> [Token] -> [(URL, URLInfo)]
expandURL url info tokens@(TagOpen name attr: ContentText text: xs)
                        | name == "a" && Maybe.isJust x       = (Maybe.fromJust x, (T.append (fst info) text, (snd info) + 1)):(expandURL url info xs)
                                where x = toAbsoluteURL url (findHref attr)
expandURL url info (x:xs)                       = expandURL url info xs
expandURL _ _ []                                = []

-- expandURL url info (TagOpen tag_name tag_attr : ContentText tag_text: xs) 
--                         | tag_name == "a"       = case x of 
--                                                         Just s -> (s, ((fst info) ++ tag_text, (snd info) + 1)):(expandURL url info xs)
--                                                         Nothing -> (expandURL url info xs)
--                                                         where
--                                                         x  = toAbsoluteURL url (findHref tag_attr)
--                         | otherwise             = expandURL url info xs
-- expandURL url info (x:xs)    = expandURL url xs
-- expandURL _ _           = []

-------------- Utilities --------------
-- Retrieve hrefs from dom element attribute
findHref :: [Attr] -> Maybe URL
findHref []     = Nothing
findHref ((Attr a b):xs)
                | a == "href"   = Just (T.unpack b)
                | otherwise     = findHref xs

-- Convert any URL (including relative URLs) to absolute URL
toAbsoluteURL :: URL -> Maybe URL -> Maybe URL
toAbsoluteURL home (Just ('.':'/':xs))                    = Just (home ++ xs)
toAbsoluteURL home (Just s@('/':xs))                      = Just (home ++ s)
toAbsoluteURL home (Just s@('h':'t':'t':'p':xs))          = Just s
toAbsoluteURL home _                                      = Nothing

-- parentURL :: URL -> URL
-- parentURL url   = (T.unpack $ x !! 0) ++ "//" ++ (T.unpack $ x !! 2) ++ "/"
--         where
--                 x = T.splitOn "/" (T.pack url)
