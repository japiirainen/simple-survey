{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Util.Database.Persist.Postgresql.Json where

import           Data.Aeson           (FromJSON, ToJSON, Value, eitherDecode,
                                       encode)
import           Data.Bifunctor       (first)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)
import           Database.Persist     (PersistField (..),
                                       PersistValue (PersistText),
                                       SqlType (SqlOther))
import           Database.Persist.Sql (PersistFieldSql (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text

newtype Json = Json Value
    deriving newtype (Show, Eq, FromJSON, ToJSON)

instance PersistField Json where
    toPersistValue = PersistText . decodeUtf8 . LBS.toStrict . encode
    fromPersistValue (PersistText text) = first Text.pack $ eitherDecode $ LBS.fromStrict $ encodeUtf8 text
    fromPersistValue _ = Left "Not PersistText"

instance PersistFieldSql Json where
    sqlType _ = SqlOther "JSON"
