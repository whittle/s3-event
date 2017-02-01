{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.S3.NotificationEvent
  ( Event(..)
  , Record(..)
  , Bucket(..)
  , RequestParameters(..)
  , ResponseElements(..)
  , S3Object(..)
  , S3Record(..)
  , UserIdentity(..)
  ) where

import ClassyPrelude
import Data.Aeson
import Data.Hex (unhex)
import qualified Network.AWS.S3.Types as A


data Event = Event
  { records :: [Record]
  } deriving (Eq, Show)

instance FromJSON Event where
  parseJSON = withObject "S3 Event" $ \o ->
    Event <$> o .: "Records"

data Record = Record
  { eventVersion :: Text
  , eventSource :: Text
  , awsRegion :: A.Region
  , eventTime :: Text
  , eventName :: Text -- A.Event?
  , userIdentity :: UserIdentity
  , requestParameters :: RequestParameters
  , responseElements :: ResponseElements
  , s3Record :: S3Record
  } deriving (Eq, Show)

instance FromJSON Record where
  parseJSON = withObject "S3 Event Record" $ \o ->
    Record <$> o .: "eventVersion"
           <*> o .: "eventSource"
           <*> o .: "awsRegion"
           <*> o .: "eventTime"
           <*> o .: "eventName"
           <*> o .: "userIdentity"
           <*> o .: "requestParameters"
           <*> o .: "responseElements"
           <*> o .: "s3"

data UserIdentity = UserIdentity
  { principalId :: Text
  } deriving (Eq, Show)

instance FromJSON UserIdentity where
  parseJSON = withObject "User Identity" $ \o ->
    UserIdentity <$> o .: "principalId"

data RequestParameters = RequestParameters
  { sourceIPAddress :: Text
  } deriving (Eq, Show)

instance FromJSON RequestParameters where
  parseJSON = withObject "Request Parameters" $ \o ->
    RequestParameters <$> o .: "sourceIPAddress"

data ResponseElements = ResponseElements
  { xAmzRequestId :: Text
  , xAmzId2 :: Text
  } deriving (Eq, Show)

instance FromJSON ResponseElements where
  parseJSON = withObject "Response Elements" $ \o ->
    ResponseElements <$> o .: "x-amz-request-id"
                     <*> o .: "x-amz-id-2"

data S3Record = S3Record
  { s3SchemaVersion :: Text
  , configurationId :: Text
  , bucket :: Bucket
  , object :: S3Object
  } deriving (Eq, Show)

instance FromJSON S3Record where
  parseJSON = withObject "S3 Record" $ \o ->
    S3Record <$> o .: "s3SchemaVersion"
             <*> o .: "configurationId"
             <*> o .: "bucket"
             <*> o .: "object"

data Bucket = Bucket
  { name :: A.BucketName
  , ownerIdentity :: UserIdentity
  , arn :: Text
  } deriving (Eq, Show)

instance FromJSON Bucket where
  parseJSON = withObject "Bucket" $ \o ->
    Bucket <$> (A.BucketName <$> o .: "name")
           <*> o .: "ownerIdentity"
           <*> o .: "arn"

data S3Object = S3Object
  { key :: A.ObjectKey
  , size :: Int
  , eTag :: A.ETag
  , versionId :: A.ObjectVersionId
  , sequencer :: Text
  } deriving (Eq, Show)

instance FromJSON S3Object where
  parseJSON = withObject "S3 Object" $ \o ->
    S3Object <$> (A.ObjectKey <$> o .: "key")
             <*> o .: "size"
             <*> (o .: "eTag" >>= unhex . encodeUtf8 >>= return . A.ETag)
             <*> (A.ObjectVersionId <$> o .: "versionId")
             <*> o .: "sequencer"
