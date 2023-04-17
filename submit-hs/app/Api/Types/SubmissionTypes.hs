{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Types.SubmissionTypes where
import Database.Beam
import Data.Aeson

data AddSubmissionBody = AddSubmissionBody
  { userId :: Int,
    assignmentId :: Int
  }
  deriving (Generic, FromJSON)
