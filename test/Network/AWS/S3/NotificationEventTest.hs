{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.S3.NotificationEventTest where

import ClassyPrelude
import Data.Aeson (decode')
import Data.Time.Clock (secondsToDiffTime)
import Network.AWS.S3.NotificationEvent
import qualified Network.AWS.S3.Types as A
import Test.Hspec
import Test.Tasty.Discover


-- http://docs.aws.amazon.com/AmazonS3/latest/dev/notification-content-structure.html
unit_deserialize_happy_face_example :: Expectation
unit_deserialize_happy_face_example = do
  raw <- readFile "examples/HappyFace.json"
  let expected = Record
        { _eventVersion = "2.0"
        , _eventSource = "aws:s3"
        , _eventAwsRegion = "us-east-1"
        , _eventTime = UTCTime (ModifiedJulianDay 40587) (secondsToDiffTime 0)
        , _eventName = "ObjectCreated:Put"
        , _eventInitiator = UserIdentity
          { _userIdentityPrincipalId = "AIDAJDPLRKLG7UEXAMPLE"
          }
        , _eventRequestParameters = RequestParameters
          { _sourceIPAddress = "127.0.0.1"
          }
        , _eventResponseElements = ResponseElements
          { _xAmzRequestId = "C3D13FE58DE4C810"
          , _xAmzId2 = "FMyUVURIY8/IgAtTv8xRjskZQpcIZ9KG4V5Wp6S7S/JRWeUWerMUE5JgHvANOjpD"
          }
        , _eventS3Record = S3Record
          { _s3SchemaVersion = "1.0"
          , _s3ConfigurationId = "testConfigRule"
          , _s3Bucket = Bucket
            { _bucketName = A.BucketName "mybucket"
            , _bucketOwner = UserIdentity
              { _userIdentityPrincipalId = "A3NL1KOZZKExample"
              }
            , _bucketARN = A.BucketName "arn:aws:s3:::mybucket"
            }
          , _s3Object = S3Object
            { _s3ObjectKey = A.ObjectKey "HappyFace.jpg"
            , _s3ObjectSize = 1024
            , _s3ObjectETag = A.ETag "\212\GS\140\217\143\NUL\178\EOT\233\128\t\152\236\248B~"
            , _s3ObjectVersionId = Just $ A.ObjectVersionId "096fKKXTRTtl3on89fVO.nfljtsv6qko"
            , _s3ObjectSequencer = Just "0055AED6DCD90281E5"
            }
          }
        }
  decode' (fromStrict raw) `shouldBe` Just (Event [expected])

unit_deserialize_user_identity :: Expectation
unit_deserialize_user_identity =
  let json = "{\"principalId\":\"AIDAJDPLRKLG7UEXAMPLE\"}"
      expected = UserIdentity "AIDAJDPLRKLG7UEXAMPLE"
  in decode' json `shouldBe` Just expected

unit_deserialize_request_parameters :: Expectation
unit_deserialize_request_parameters =
  let json = "{\"sourceIPAddress\":\"127.0.0.1\"}"
      expected = RequestParameters "127.0.0.1"
  in decode' json `shouldBe` Just expected

unit_deserialize_response_elements :: Expectation
unit_deserialize_response_elements =
  let json = "{\"x-amz-request-id\":\"C3D13FE58DE4C810\",\"x-amz-id-2\":\"FMyUVURIY8/IgAtTv8xRjskZQpcIZ9KG4V5Wp6S7S/JRWeUWerMUE5JgHvANOjpD\"}"
      expected = ResponseElements "C3D13FE58DE4C810" "FMyUVURIY8/IgAtTv8xRjskZQpcIZ9KG4V5Wp6S7S/JRWeUWerMUE5JgHvANOjpD"
  in decode' json `shouldBe` Just expected

unit_deserialize_S3_record :: Expectation
unit_deserialize_S3_record =
  let json = "{\"s3SchemaVersion\":\"1.0\",\"configurationId\":\"testConfigRule\",\"bucket\":{\"name\":\"mybucket\",\"ownerIdentity\":{\"principalId\":\"A3NL1KOZZKExample\"},\"arn\":\"arn:aws:s3:::mybucket\"},\"object\":{\"key\":\"HappyFace.jpg\",\"size\":1024,\"eTag\":\"d41d8cd98f00b204e9800998ecf8427e\",\"versionId\":\"096fKKXTRTtl3on89fVO.nfljtsv6qko\",\"sequencer\":\"0055AED6DCD90281E5\"}}"
      expected = S3Record "1.0" "testConfigRule" (Bucket "mybucket" (UserIdentity "A3NL1KOZZKExample") (A.BucketName "arn:aws:s3:::mybucket")) (S3Object (A.ObjectKey "HappyFace.jpg") 1024 (A.ETag "\212\GS\140\217\143\NUL\178\EOT\233\128\t\152\236\248B~") (Just $ A.ObjectVersionId "096fKKXTRTtl3on89fVO.nfljtsv6qko") (Just "0055AED6DCD90281E5"))
  in decode' json `shouldBe` Just expected

unit_deserialize_bucket :: Expectation
unit_deserialize_bucket =
  let json = "{\"name\":\"mybucket\",\"ownerIdentity\":{\"principalId\":\"A3NL1KOZZKExample\"},\"arn\":\"arn:aws:s3:::mybucket\"}"
      expected = Bucket (A.BucketName "mybucket") (UserIdentity "A3NL1KOZZKExample") (A.BucketName "arn:aws:s3:::mybucket")
  in decode' json `shouldBe` Just expected

unit_deserialize_S3_object :: Expectation
unit_deserialize_S3_object =
  let json = "{\"key\":\"HappyFace.jpg\",\"size\":1024,\"eTag\":\"d41d8cd98f00b204e9800998ecf8427e\",\"versionId\":\"096fKKXTRTtl3on89fVO.nfljtsv6qko\",\"sequencer\":\"0055AED6DCD90281E5\"}"
      expected = S3Object (A.ObjectKey "HappyFace.jpg") 1024 (A.ETag "\212\GS\140\217\143\NUL\178\EOT\233\128\t\152\236\248B~") (Just $ A.ObjectVersionId "096fKKXTRTtl3on89fVO.nfljtsv6qko") (Just "0055AED6DCD90281E5")
  in decode' json `shouldBe` Just expected
