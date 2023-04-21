{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types.AttemptTypes where

import Data.Aeson
import Data.Text
import Database.Beam

data AddAttemptBody = AddAttemptBody
  { submissionId :: Int,
    file :: Text
  }
  deriving (Generic, FromJSON)
