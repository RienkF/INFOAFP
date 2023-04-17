{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Types.AttemptTypes where
import Database.Beam
import Data.Aeson
import Data.Text

data AddAttemptBody = AddAttemptBody
  { submissionId :: Int,
    file :: Text
  }
  deriving (Generic, FromJSON)

-- TODO: DelAttemptBody = ..
