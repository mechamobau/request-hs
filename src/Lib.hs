{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( middlewareRequest
    ) where

import Prelude as P

import Data.Map.Strict as STS

import Data.Maybe (fromMaybe)

import Data.Bifunctor (second)

import Control.Monad.IO.Class

import Data.ByteString.Lazy.UTF8 as BSU

import Data.Text

import Data.List as L

import Data.Aeson as AS
import Network.HTTP.Req

import Types.BTC
import qualified Data.Bifunctor

listOfCurrencyLabels :: [(String, String)]
listOfCurrencyLabels = [
    ("USD", "United States Dollar"),
    ("BRL", "Brazilian Real"),
    ("EUR", "Euro"),
    ("CAD", "Canadian Dollar"),
    ("BTC", "Bitcoin")
  ]

calculateRateFloat :: Float -> (String, Float) -> (Text, Currency)
calculateRateFloat btcRate (code, rate) =  (textCode, Currency textCode rateText description rateFloat)
  where
    textCode = pack code
    rateFloat = rate * btcRate
    rateText = pack $ show rateFloat
    description = pack (fromMaybe "" (getCurrencyLabelByCode code))

getCurrencyLabelByCode :: String -> Maybe String
getCurrencyLabelByCode code = fmap snd (L.find (\(code',_) -> code' == code) listOfCurrencyLabels)

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


middlewareRequest :: IO [(Text, Currency)]
middlewareRequest = do 
  file <- readFileCurrencies

  runReq defaultHttpConfig $ do
    response <- req GET (https "api.coindesk.com" /: "v1" /: "bpi" /: "currentprice" /: "BTC.json") NoReqBody jsonResponse mempty

    let (BTC _ _ (BPI bpi) ) = (responseBody response :: BTC)

    let mUsd = STS.lookup "USD" bpi

    case mUsd of
      Just usd -> do
        let mBtc = STS.lookup "BTC" bpi
        
        case mBtc of
          Just btc -> do
            let (Currency _ _ _ btcRate) = usd
            let baseList' = P.map (calculateRateFloat btcRate) file

            liftIO (return (("USD", usd) : ("BTC", btc) : baseList'))
          _ -> error "Moeda BTC não retornado pela API"
      _ -> error "Moeda USD não retornado pela API"