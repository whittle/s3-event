module Network.AWS.S3.NotificationEvent
  ( Record(..)
  ) where

import Data.Aeson


data Record = Record
  deriving (Eq, Show)

instance FromJSON Record where
  parseJSON _ = pure Record
