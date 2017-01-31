{-# LANGUAGE NoImplicitPrelude #-}

module Network.AWS.S3.NotificationEventTest where

import ClassyPrelude
import Data.Aeson (decode')
import Network.AWS.S3.NotificationEvent
import Test.Tasty.Discover


-- http://docs.aws.amazon.com/AmazonS3/latest/dev/notification-content-structure.html
case_deserialize_happy_face :: Expectation
case_deserialize_happy_face = do
  raw <- readFile "examples/HappyFace.json"
  decode' raw `shouldBe` Just Record
