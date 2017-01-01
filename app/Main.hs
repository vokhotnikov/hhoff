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

import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
  r <- loadPage "https://sevastopol.hh.ru/search/resume?area=130&clusters=true&text=C%23&pos=full_text&logic=normal&exp_period=all_time"
  mapM_ putStrLn r

loadPage :: String -> IO [String]
loadPage url = do
    r <- getPage url
    (links, maybeNext) <- parsePage . C.unpack $ r ^. responseBody
    maybeRest <- loadPage `mapM` (maybeNext >>= resolveRelativeUrl)
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
      
getPage url = do
    res <- getWith opts url
    threadDelay 1234567
    return res
  where opts = defaults & header "User-Agent" .~ ["Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:50.0) Gecko/20100101 Firefox/50.0"]
