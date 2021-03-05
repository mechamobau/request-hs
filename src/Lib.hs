{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( makeRequest
    ) where

import Prelude as P

import Data.Map.Strict as STS

import Data.Bifunctor (second)

import Control.Monad.IO.Class

import Data.ByteString.Lazy.UTF8 as BSU

import Data.Aeson as AS
import Network.HTTP.Req

import Types.BTC
import qualified Data.Bifunctor

convertToFloat :: String -> Float
convertToFloat = read

readFileCurrencies :: IO [(String, Float)]
readFileCurrencies = do
  currencies <- readFile "assets/currencies.json"

  let mJson = AS.decode $ BSU.fromString currencies :: Maybe (Map String String)

  case mJson of
    Just currencies' -> do
      let currenciesList = STS.toList currencies'

      return (P.map (second convertToFloat) currenciesList)
    _   -> error "Arquivo com formato incompatível"


makeRequest :: IO ()
makeRequest = do 
  file <- readFileCurrencies

  runReq defaultHttpConfig $ do

    liftIO $ print file

    response <- req GET (https "api.coindesk.com" /: "v1" /: "bpi" /: "currentprice" /: "BTC.json") NoReqBody jsonResponse mempty

    let btc' = (responseBody response :: BTC)

    liftIO $ print btc'
