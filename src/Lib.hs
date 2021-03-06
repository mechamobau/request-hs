{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( middlewareRequest
    ) where

import Data.Aeson as AS ( decode, encode )
import Data.Bifunctor ( Bifunctor(second, first) )
import Data.ByteString.Lazy.UTF8 as BSU ( fromString, toString )
import Data.List as L ( (++), find )
import Data.Map.Strict as STS ( lookup, toList, fromList, Map )
import Data.Maybe (fromMaybe)
import Data.Text ( pack, Text )
import Network.HTTP.Req
    ( (/:),
      defaultHttpConfig,
      https,
      jsonResponse,
      req,
      responseBody,
      runReq,
      GET(GET),
      NoReqBody(NoReqBody) )
import Prelude as P
import Types.BTC ( BTC(BTC), BPI(BPI), Currency(Currency) )

data AvailableCurrencies = EUR | BRL | CAD deriving Show

data CurrencyValue = CurrencyValue { currency :: AvailableCurrencies, value :: Float  }

pathFileCurrencies :: FilePath
pathFileCurrencies = "assets/currencies.json"

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

stringToFloat :: String -> Float
stringToFloat = read

floatToString :: Float -> String
floatToString = show

readFileCurrencies :: IO [(String, Float)]
readFileCurrencies = do
  currencies <- readFile pathFileCurrencies

  let mJson = AS.decode $ BSU.fromString currencies :: Maybe (Map String String)

  case mJson of
    Just currencies' -> do
      let currenciesList = STS.toList currencies'

      return (P.map (second stringToFloat) currenciesList)
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
        let placeholderBtc = pack (fromMaybe "" (getCurrencyLabelByCode "BTC"))

        let btc = fromMaybe (Currency "BTC" "1" placeholderBtc 1) (STS.lookup "BTC" bpi)

        let (Currency _ _ _ btcRate) = usd
        let baseList' = P.map (calculateRateFloat btcRate) file

        let newList  = STS.fromList (P.map (Data.Bifunctor.first show) baseList')

        return (("USD", usd) : ("BTC", btc) : baseList')
      _ -> error "Moeda USD não retornado pela API"

parseValuesToString :: [(String, Float)] -> [(String, String)]
parseValuesToString = P.map (second floatToString)

setCurrencyFileValue :: CurrencyValue -> IO (Either String Bool)
setCurrencyFileValue (CurrencyValue code newvalue) = do
    currencies <- readFileCurrencies

    let mCurrency = L.find (\i -> fst i == show code) currencies

    let parseFile x = BSU.toString (encode $ fromList (parseValuesToString x))

    case mCurrency of
      Just _ -> do
        if newvalue < 0 then 
          return $ Left "Valor não pode ser negativo"
        else do
          let changeCurrencyValue (x, y) = (x, if x == show code then newvalue else y)

          let newFile = parseFile (P.map changeCurrencyValue currencies)
          
          writeFile pathFileCurrencies newFile

          return $ Right True
      _ -> do
        let newFile = parseFile (currencies ++ [(show code, newvalue)])

        writeFile pathFileCurrencies newFile

        return $ Right True
