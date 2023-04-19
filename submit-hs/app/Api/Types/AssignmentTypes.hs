{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types.AssignmentTypes where

import Api.Types.SubmissionTypes (AddSubmissionBody (assignmentId))
import Data.Aeson
import Data.Aeson.Types
import Data.Int (Int32)
import Data.Text
import Data.Time (LocalTime)
import Database.Model
import GHC.Generics

data AddAssignmentBody = AddAssignmentBody
  { assignmentStartDate :: LocalTime,
    assignmentDeadline :: LocalTime,
    assignmentDescription :: Text,
    assignmentWeight :: Double,
    assignmentClassroom :: Int
  }
  deriving (Generic, FromJSON)