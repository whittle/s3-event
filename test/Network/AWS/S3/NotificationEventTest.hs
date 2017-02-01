{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.AWS.S3.NotificationEventTest where

import ClassyPrelude
import Data.Aeson (decode')
import Network.AWS.S3.NotificationEvent
import qualified Network.AWS.S3.Types as A
import Test.Tasty.Discover


-- http://docs.aws.amazon.com/AmazonS3/latest/dev/notification-content-structure.html
case_deserialize_happy_face_example :: Expectation
case_deserialize_happy_face_example = do
  raw <- readFile "examples/HappyFace.json"
  let expected = Record
        { eventVersion = "2.0"
        , eventSource = "aws:s3"
        , awsRegion = A.NorthVirginia
        , eventTime = "1970-01-01T00:00:00.000Z"
        , eventName = "ObjectCreated:Put"
        , userIdentity = UserIdentity
          { principalId = "AIDAJDPLRKLG7UEXAMPLE"
          }
        , requestParameters = RequestParameters
          { sourceIPAddress = "127.0.0.1"
          }
        , responseElements = ResponseElements
          { xAmzRequestId = "C3D13FE58DE4C810"
          , xAmzId2 = "FMyUVURIY8/IgAtTv8xRjskZQpcIZ9KG4V5Wp6S7S/JRWeUWerMUE5JgHvANOjpD"
          }
        , s3Record = S3Record
          { s3SchemaVersion = "1.0"
          , configurationId = "testConfigRule"
          , bucket = Bucket
            { name = A.BucketName "mybucket"
            , ownerIdentity = UserIdentity
              { principalId = "A3NL1KOZZKExample"
              }
            , arn = "arn:aws:s3:::mybucket"
            }
          , object = S3Object
            { key = A.ObjectKey "HappyFace.jpg"
            , size = 1024
            , eTag = A.ETag "\212\GS\140\217\143\NUL\178\EOT\233\128\t\152\236\248B~"
            , versionId = A.ObjectVersionId "096fKKXTRTtl3on89fVO.nfljtsv6qko"
            , sequencer = "0055AED6DCD90281E5"
            }
          }
        }
  decode' (fromStrict raw) `shouldBe` Just (Event [expected])

case_deserialize_user_identity :: Expectation
case_deserialize_user_identity =
  let json = "{\"principalId\":\"AIDAJDPLRKLG7UEXAMPLE\"}"
      expected = UserIdentity "AIDAJDPLRKLG7UEXAMPLE"
  in decode' json `shouldBe` Just expected

case_deserialize_request_parameters :: Expectation
case_deserialize_request_parameters =
  let json = "{\"sourceIPAddress\":\"127.0.0.1\"}"
      expected = RequestParameters "127.0.0.1"
  in decode' json `shouldBe` Just expected

case_deserialize_response_elements :: Expectation
case_deserialize_response_elements =
  let json = "{\"x-amz-request-id\":\"C3D13FE58DE4C810\",\"x-amz-id-2\":\"FMyUVURIY8/IgAtTv8xRjskZQpcIZ9KG4V5Wp6S7S/JRWeUWerMUE5JgHvANOjpD\"}"
      expected = ResponseElements "C3D13FE58DE4C810" "FMyUVURIY8/IgAtTv8xRjskZQpcIZ9KG4V5Wp6S7S/JRWeUWerMUE5JgHvANOjpD"
  in decode' json `shouldBe` Just expected

case_deserialize_S3_record :: Expectation
case_deserialize_S3_record =
  let json = "{\"s3SchemaVersion\":\"1.0\",\"configurationId\":\"testConfigRule\",\"bucket\":{\"name\":\"mybucket\",\"ownerIdentity\":{\"principalId\":\"A3NL1KOZZKExample\"},\"arn\":\"arn:aws:s3:::mybucket\"},\"object\":{\"key\":\"HappyFace.jpg\",\"size\":1024,\"eTag\":\"d41d8cd98f00b204e9800998ecf8427e\",\"versionId\":\"096fKKXTRTtl3on89fVO.nfljtsv6qko\",\"sequencer\":\"0055AED6DCD90281E5\"}}"
      expected = S3Record "1.0" "testConfigRule" (Bucket "mybucket" (UserIdentity "A3NL1KOZZKExample") "arn:aws:s3:::mybucket") (S3Object (A.ObjectKey "HappyFace.jpg") 1024 (A.ETag "\212\GS\140\217\143\NUL\178\EOT\233\128\t\152\236\248B~") (A.ObjectVersionId "096fKKXTRTtl3on89fVO.nfljtsv6qko") "0055AED6DCD90281E5")
  in decode' json `shouldBe` Just expected

case_deserialize_bucket :: Expectation
case_deserialize_bucket =
  let json = "{\"name\":\"mybucket\",\"ownerIdentity\":{\"principalId\":\"A3NL1KOZZKExample\"},\"arn\":\"arn:aws:s3:::mybucket\"}"
      expected = Bucket (A.BucketName "mybucket") (UserIdentity "A3NL1KOZZKExample") "arn:aws:s3:::mybucket"
  in decode' json `shouldBe` Just expected

case_deserialize_S3_object :: Expectation
case_deserialize_S3_object =
  let json = "{\"key\":\"HappyFace.jpg\",\"size\":1024,\"eTag\":\"d41d8cd98f00b204e9800998ecf8427e\",\"versionId\":\"096fKKXTRTtl3on89fVO.nfljtsv6qko\",\"sequencer\":\"0055AED6DCD90281E5\"}"
      expected = S3Object (A.ObjectKey "HappyFace.jpg") 1024 (A.ETag "\212\GS\140\217\143\NUL\178\EOT\233\128\t\152\236\248B~") (A.ObjectVersionId "096fKKXTRTtl3on89fVO.nfljtsv6qko") "0055AED6DCD90281E5"
  in decode' json `shouldBe` Just expected
