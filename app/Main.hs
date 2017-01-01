{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Wreq
import Control.Lens
import Control.Concurrent (threadDelay)
import Data.Aeson.Lens (_String, key)
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.List.Split
import Data.Maybe (fromMaybe)
import Network.URI
import Safe
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy.UTF8 as U8
import qualified Data.ByteString.Char8 as C
import qualified Network.Wreq.Session as S

main :: IO ()
main = do
  args <- getArgs
  withLogin (head args) (args !! 1) $ \sess -> do
    ids <- readIds sess $ drop 3 args
    mapM_ (downloadResumeForId sess $ args !! 2) $ take 3 ids
    mapM_ putStrLn ids

downloadResumeForId :: S.Session -> String -> String -> IO ()
downloadResumeForId sess apiKey id = do
    r <- getApiPage sess apiKey $ "https://api.hh.ru/resumes/" ++ id
    writeFile outFileName $ U8.toString $ r ^. responseBody
  where
    outFileName = id ++ ".json"

readIds :: S.Session -> [String] -> IO [String]
readIds sess [] = do
    ids <- loadPage sess "https://sevastopol.hh.ru/search/resume?area=130&clusters=true&text=C%23&pos=full_text&logic=normal&exp_period=all_time"
    writeFile "ids.txt" $ unlines ids
    return ids
readIds _ [file] = lines `fmap` readFile file

loadPage :: S.Session -> String -> IO [String]
loadPage sess url = do
    r <- getPage sess url
    (links, maybeNext) <- parsePage . U8.toString $ r ^. responseBody
    maybeRest <- loadPage sess `mapM` (maybeNext >>= resolveRelativeUrl)
    return $ (extractId `fmap` links) ++ fromMaybe [] maybeRest
  where
        resolveRelativeUrl :: String -> Maybe String
        resolveRelativeUrl rel = do
          base <- parseURI url
          uri <- parseURIReference rel
          return $ (uriToString id . flip relativeTo base) uri ""

extractId :: String -> String
extractId = (!!2) . splitOneOf "/?"

parsePage :: String -> IO ([String], Maybe String)
parsePage body = do
        results <- runX $ html >>> css "td.output__main-cell a.output__name" ! "href"
        nextLinks <- runX $ html >>> css "a.HH-Pager-Controls-Next" ! "href"
        return (results, headMay nextLinks)
  where html = parseHtml body

parseXsrfToken body = runX $ html >>> css "input[name~=_xsrf]" ! "value"
  where html = parseHtml body

withLogin login password action = S.withSession $ \sess -> do
    r <- S.getWith httpOptions sess "https://sevastopol.hh.ru/"
    xsrf <- parseXsrfToken . U8.toString $ r ^. responseBody 
    S.postWith httpOptions sess "https://sevastopol.hh.ru/account/login?backurl=%2F" ["username" := login, "password" := password, "_xsrf" := head xsrf]
    action sess

getApiPage sess key url = do
    res <- S.getWith apiOptions sess url
    threadDelay 234567
    return res
  where apiOptions = httpOptions & header "Authorization" .~ [C.pack $ "Bearer " ++ key]

getPage sess url = do
    res <- S.getWith httpOptions sess url
    threadDelay 1234567
    return res

httpOptions = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:50.0) Gecko/20100101 Firefox/50.0"]


