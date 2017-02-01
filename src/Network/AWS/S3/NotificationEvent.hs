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
  -- ^ Always "2.0"
  , eventSource :: Text
  -- ^ Always "aws:s3"
  , awsRegion :: A.Region
  -- ^ Always "us-east-1"
  , eventTime :: UTCTime
  -- ^ The time when S3 finished processing the request
  , eventName :: Text -- A.Event?
  -- ^ Type of event that took place
  , userIdentity :: UserIdentity
  -- ^ User who caused the event
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
  -- ^ Amazon customer ID
  } deriving (Eq, Show)

instance FromJSON UserIdentity where
  parseJSON = withObject "User Identity" $ \o ->
    UserIdentity <$> o .: "principalId"

data RequestParameters = RequestParameters
  { sourceIPAddress :: Text
  -- ^ IP address where request came from
  } deriving (Eq, Show)

instance FromJSON RequestParameters where
  parseJSON = withObject "Request Parameters" $ \o ->
    RequestParameters <$> o .: "sourceIPAddress"

data ResponseElements = ResponseElements
  { xAmzRequestId :: Text
  -- ^ Amazon S3 generated request ID
  , xAmzId2 :: Text
  -- ^ Amazon S3 host that processed the request
  } deriving (Eq, Show)

instance FromJSON ResponseElements where
  parseJSON = withObject "Response Elements" $ \o ->
    ResponseElements <$> o .: "x-amz-request-id"
                     <*> o .: "x-amz-id-2"

data S3Record = S3Record
  { s3SchemaVersion :: Text
  -- ^ Always "1.0"
  , s3ConfigurationId :: Text
  -- ^ ID found in the bucket notification configuration
  , s3Bucket :: Bucket
  , s3Object :: S3Object
  } deriving (Eq, Show)

instance FromJSON S3Record where
  parseJSON = withObject "S3 Record" $ \o ->
    S3Record <$> o .: "s3SchemaVersion"
             <*> o .: "configurationId"
             <*> o .: "bucket"
             <*> o .: "object"

data Bucket = Bucket
  { bucketName :: A.BucketName
  , bucketOwnerIdentity :: UserIdentity
  , bucketARN :: A.BucketName
  } deriving (Eq, Show)

instance FromJSON Bucket where
  parseJSON = withObject "Bucket" $ \o ->
    Bucket <$> (A.BucketName <$> o .: "name")
           <*> o .: "ownerIdentity"
           <*> (A.BucketName <$> o .: "arn")

data S3Object = S3Object
  { s3ObjectKey :: A.ObjectKey
  , s3ObjectSize :: Int
  , s3ObjectETag :: A.ETag
  , s3ObjectVersionId :: A.ObjectVersionId
  -- ^ object version if bucket is versioning-enabled, otherwise null
  , s3ObjectSequencer :: Text
  -- ^ a string representation of a hexadecimal value used to
  -- determine event sequence, only used with PUTs and DELETEs
  } deriving (Eq, Show)

instance FromJSON S3Object where
  parseJSON = withObject "S3 Object" $ \o ->
    S3Object <$> (A.ObjectKey <$> o .: "key")
             <*> o .: "size"
             <*> (o .: "eTag" >>= unhex . encodeUtf8 >>= return . A.ETag)
             <*> (A.ObjectVersionId <$> o .: "versionId")
             <*> o .: "sequencer"
