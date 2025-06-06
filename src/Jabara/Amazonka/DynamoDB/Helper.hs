{-# LANGUAGE OverloadedStrings #-}

module Jabara.Amazonka.DynamoDB.Helper (
  PropertyName,
  DynamoDBRecord,
  FromAttributeValue (..),
  ToAttributeValue (..),
  ToDynamoDBRecord (..),
  fromAttributeValueUnsafe,
  getBoolean,
  getBooleanUnsafe,
  getInteger,
  getIntegerUnsafe,
  getText,
  getTextUnsafe,
  getUtcTime,
  getUtcTimeUnsafe,
  utcTimeToText,
  getList,
  getListUnsafe,
  getMap,
  getMapUnsafe,
) where

import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.Prelude (HashMap)
import Control.Exception.Safe (throwString)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.ISO8601
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

type PropertyName = Text
type TypeName = Text
type DynamoDBRecord = HashMap PropertyName AttributeValue

class FromAttributeValue a where
  fromAttributeValue :: DynamoDBRecord -> Either Text a

fromAttributeValueUnsafe :: (FromAttributeValue a) => DynamoDBRecord -> IO a
fromAttributeValueUnsafe av =
  case fromAttributeValue av of
    Right v -> return v
    Left e -> throwString $ Text.unpack e

class ToAttributeValue a where
  toAttributeValue :: a -> AttributeValue

class ToDynamoDBRecord a where
  toDynamoDBRecord :: a -> DynamoDBRecord

getUtcTime :: DynamoDBRecord -> PropertyName -> Either Text UTCTime
getUtcTime values propertyName =
  getValueInternal
    values
    propertyName
    ( \av -> case av of
        S b -> case parseISO8601 $ Text.unpack b of
          Just t -> Right t
          Nothing -> Left $ Text.concat ["The value of property '", propertyName, "' is not in ISO8601 format"]
        _ -> Left $ differentType propertyName "UTCTime"
    )

getUtcTimeUnsafe :: DynamoDBRecord -> PropertyName -> IO UTCTime
getUtcTimeUnsafe values propertyName =
  case getUtcTime values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e

utcTimeToText :: UTCTime -> Text
utcTimeToText = Text.pack . formatISO8601

getText :: DynamoDBRecord -> PropertyName -> Either Text Text
getText values propertyName =
  getValueInternal
    values
    propertyName
    ( \av -> case av of
        S b -> Right b
        _ -> Left $ differentType propertyName "Text"
    )

getTextUnsafe :: DynamoDBRecord -> PropertyName -> IO Text
getTextUnsafe values propertyName =
  case getText values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e

getInteger :: DynamoDBRecord -> PropertyName -> Either Text Integer
getInteger values propertyName =
  getValueInternal
    values
    propertyName
    ( \av -> case av of
        N b -> Right $ Prelude.read $ Text.unpack b
        _ -> Left $ differentType propertyName "Integer"
    )

getIntegerUnsafe :: DynamoDBRecord -> PropertyName -> IO Integer
getIntegerUnsafe values propertyName =
  case getInteger values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e

getBoolean :: DynamoDBRecord -> PropertyName -> Either Text Bool
getBoolean values propertyName =
  getValueInternal
    values
    propertyName
    ( \av -> case av of
        BOOL b -> Right b
        _ -> Left $ differentType propertyName "Bool"
    )

getBooleanUnsafe :: DynamoDBRecord -> PropertyName -> IO Bool
getBooleanUnsafe values propertyName =
  case getBoolean values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e

getMap :: DynamoDBRecord -> PropertyName -> Either Text (Map Text AttributeValue)
getMap values propertyName =
  getValueInternal
    values
    propertyName
    ( \av -> case av of
        M v -> Right v
        _ -> Left $ differentType propertyName "Map"
    )

getMapUnsafe :: DynamoDBRecord -> PropertyName -> IO (Map Text AttributeValue)
getMapUnsafe values propertyName =
  case getMap values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e

getList :: DynamoDBRecord -> PropertyName -> Either Text (Vector AttributeValue)
getList values propertyName =
  getValueInternal
    values
    propertyName
    ( \av -> case av of
        L v -> Right v
        _ -> Left $ differentType propertyName "List"
    )

getListUnsafe :: DynamoDBRecord -> PropertyName -> IO (Vector AttributeValue)
getListUnsafe values propertyName =
  case getList values propertyName of
    Right s -> return s
    Left e -> throwString $ Text.unpack e

getValueInternal ::
  DynamoDBRecord ->
  PropertyName ->
  (AttributeValue -> Either Text a) ->
  Either Text a
getValueInternal values propertyName toValue =
  case HashMap.lookup propertyName values of
    Just av ->
      case toValue av of
        Right v -> Right v
        Left e -> Left e
    Nothing -> Left $ Text.concat ["property '", propertyName, "' not found."]

differentType :: PropertyName -> TypeName -> Text
differentType propertyName typeName = Text.concat ["The value of property '", propertyName, "' is different type from the expected type ", typeName, "."]
