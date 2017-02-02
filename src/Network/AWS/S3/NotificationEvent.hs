{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.AWS.S3.NotificationEvent
  ( Event(..)
  , records
  , Record(..)
  , eventVersion
  , eventSource
  , eventAwsRegion
  , eventTime
  , eventName
  , eventInitiator
  , eventRequestParameters
  , eventResponseElements
  , eventS3Record
  , Bucket(..)
  , bucketName
  , bucketOwner
  , bucketARN
  , RequestParameters(..)
  , sourceIPAddress
  , ResponseElements(..)
  , xAmzId2
  , xAmzRequestId
  , S3Object(..)
  , s3ObjectKey
  , s3ObjectSize
  , s3ObjectETag
  , s3ObjectVersionId
  , s3ObjectSequencer
  , S3Record(..)
  , s3SchemaVersion
  , s3ConfigurationId
  , s3Bucket
  , s3Object
  , UserIdentity(..)
  , userIdentityPrincipalId
  ) where

import ClassyPrelude
import Control.Lens
import Data.Aeson
import Data.Hex (unhex)
import qualified Network.AWS.S3.Types as A


data Event = Event
  { _records :: [Record]
  } deriving (Eq, Show)

instance FromJSON Event where
  parseJSON = withObject "S3 Event" $ \o ->
    Event <$> o .: "Records"

data Record = Record
  { _eventVersion :: Text
  -- ^ Always "2.0"
  , _eventSource :: Text
  -- ^ Always "aws:s3"
  , _eventAwsRegion :: Text
  -- ^ Always "us-east-1"
  -- TODO: once amazonka >= 1.5.4, use A.Region
  , _eventTime :: UTCTime
  -- ^ The time when S3 finished processing the request
  , _eventName :: Text
  -- ^ Type of event that took place
  -- TODO: consider using A.Event
  , _eventInitiator :: UserIdentity
  -- ^ User who caused the event
  , _eventRequestParameters :: RequestParameters
  , _eventResponseElements :: ResponseElements
  , _eventS3Record :: S3Record
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
  { _userIdentityPrincipalId :: Text
  -- ^ Amazon customer ID
  } deriving (Eq, Show)

instance FromJSON UserIdentity where
  parseJSON = withObject "User Identity" $ \o ->
    UserIdentity <$> o .: "principalId"

data RequestParameters = RequestParameters
  { _sourceIPAddress :: Text
  -- ^ IP address where request came from
  } deriving (Eq, Show)

instance FromJSON RequestParameters where
  parseJSON = withObject "Request Parameters" $ \o ->
    RequestParameters <$> o .: "sourceIPAddress"

data ResponseElements = ResponseElements
  { _xAmzRequestId :: Text
  -- ^ Amazon S3 generated request ID
  , _xAmzId2 :: Text
  -- ^ Amazon S3 host that processed the request
  } deriving (Eq, Show)

instance FromJSON ResponseElements where
  parseJSON = withObject "Response Elements" $ \o ->
    ResponseElements <$> o .: "x-amz-request-id"
                     <*> o .: "x-amz-id-2"

data S3Record = S3Record
  { _s3SchemaVersion :: Text
  -- ^ Always "1.0"
  , _s3ConfigurationId :: Text
  -- ^ ID found in the bucket notification configuration
  , _s3Bucket :: Bucket
  , _s3Object :: S3Object
  } deriving (Eq, Show)

instance FromJSON S3Record where
  parseJSON = withObject "S3 Record" $ \o ->
    S3Record <$> o .: "s3SchemaVersion"
             <*> o .: "configurationId"
             <*> o .: "bucket"
             <*> o .: "object"

data Bucket = Bucket
  { _bucketName :: A.BucketName
  , _bucketOwner :: UserIdentity
  , _bucketARN :: A.BucketName
  } deriving (Eq, Show)

instance FromJSON Bucket where
  parseJSON = withObject "Bucket" $ \o ->
    Bucket <$> (A.BucketName <$> o .: "name")
           <*> o .: "ownerIdentity"
           <*> (A.BucketName <$> o .: "arn")

data S3Object = S3Object
  { _s3ObjectKey :: A.ObjectKey
  , _s3ObjectSize :: Int
  , _s3ObjectETag :: A.ETag
  , _s3ObjectVersionId :: Maybe A.ObjectVersionId
  -- ^ Object version if bucket is versioning-enabled, otherwise null
  , _s3ObjectSequencer :: Maybe Text
  -- ^ A string representation of a hexadecimal value used to
  -- determine event sequence, only used with PUTs and DELETEs
  } deriving (Eq, Show)

instance FromJSON S3Object where
  parseJSON = withObject "S3 Object" $ \o ->
    S3Object <$> (A.ObjectKey <$> o .: "key")
             <*> o .: "size"
             <*> (o .: "eTag" >>= unhex . encodeUtf8 >>= return . A.ETag)
             <*> ((map . map) A.ObjectVersionId (o .:? "versionId"))
             <*> o .:? "sequencer"

$(makeLenses ''Event)
$(makeLenses ''Record)
$(makeLenses ''UserIdentity)
$(makeLenses ''RequestParameters)
$(makeLenses ''ResponseElements)
$(makeLenses ''S3Record)
$(makeLenses ''Bucket)
$(makeLenses ''S3Object)
