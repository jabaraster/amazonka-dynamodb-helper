{-# LANGUAGE OverloadedStrings #-}

module Jabara.Amazonka.DynamoDB.Helper (
  getText,
  getTextUnsafe,
  getInteger,
  getIntegerUnsafe,
) where

import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.Prelude (HashMap)
import Control.Exception.Safe (throwString)
import qualified Data.HashMap.Strict as HashMap
import Data.Text (Text)
import qualified Data.Text as Text

getText :: HashMap Text AttributeValue -> Text -> Either Text Text
getText values propertyName =
  case HashMap.lookup propertyName values of
    Just (S s) -> Right s
    _ -> Left $ Text.concat ["property '", propertyName, "' not found."]

getTextUnsafe :: HashMap Text AttributeValue -> Text -> IO Text
getTextUnsafe values propertyName =
  case getText values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e

getInteger :: HashMap Text AttributeValue -> Text -> Either Text Integer
getInteger values propertyName =
  case HashMap.lookup propertyName values of
    Just (N s) -> Right $ read $ Text.unpack s
    _ -> Left $ Text.concat ["property '", propertyName, "' not found."]

getIntegerUnsafe :: HashMap Text AttributeValue -> Text -> IO Integer
getIntegerUnsafe values propertyName =
  case getInteger values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e
