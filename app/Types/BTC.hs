{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.BTC where
 
import Data.Aeson

import Data.Char

import Data.Text as T

import Generics.Deriving.Base
-- | Tipo retornado para moedas
data Currency = Currency 
            { 
              code        :: T.Text
            , rate        :: T.Text
            , description :: T.Text
            , rateFloat   :: Float } deriving Show

instance FromJSON Currency where
  parseJSON (Object v) = 
    Currency  <$> v .: "code"
              <*> v .: "rate"
              <*> v .: "description"
              <*> v .: "rate_float"

instance ToJSON Currency where
  toJSON (Currency code rate description rateFloat) =
    object [
        "code"        .= code
      , "rate"        .= rate
      , "description" .= description
      , "rate_float"  .= rateFloat
    ]

data Time = Time
            {
              updated     ::  T.Text
            , updatedISO  :: T.Text
            , updatedUk   :: T.Text
            } deriving (Show)

instance FromJSON Time where
  parseJSON (Object v) =
    Time  <$> v .: "updated"
          <*> v .: "updatedISO"
          <*> v .: "updateduk"

instance ToJSON Time where
  toJSON (Time updated updatedISO updatedUk) =
    object [
        "updated"     .= updated
      , "updatedISO"  .= updatedISO
      , "updateduk"   .= updatedUk
    ]

-- | Bitcoin Price Index (BPI)
data BPI = BPI
            {
                usd :: Currency
              , btc :: Currency
            } deriving (Show)

instance FromJSON BPI where
  parseJSON (Object v) =
    BPI <$> v .: "USD"
        <*> v .: "BTC"

instance ToJSON BPI where
  toJSON (BPI usd btc) =
    object [
        "USD" .= usd
      , "BTC" .= btc
    ]

data BTC = BTC
            {
                time        :: Time
              , disclaimer  :: T.Text
              , bpi         :: BPI
            } deriving (Show, Generic)

instance FromJSON BTC where
  parseJSON (Object v) =
    BTC <$> v .: "time"
        <*> v .: "disclaimer"
        <*> v .: "bpi"

instance ToJSON BTC where
  toJSON (BTC time disclaimer bpi) =
    object [
        "time" .= time
      , "disclaimer" .= disclaimer
      , "bpi" .= bpi
    ]