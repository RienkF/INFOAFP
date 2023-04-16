{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Api.Types.AssignmentTypes where

import GHC.Generics
import Database.Model
import Data.Aeson
import Data.Aeson.Types
import Api.Types.SubmissionTypes (AddSubmissionBody(assignmentId))
import Data.Time (LocalTime)
import Data.Int (Int32)
import Data.Text

data AddAssignmentBody = AddAssignmentBody
  { assignmentStartDate :: LocalTime,
    assignmentDeadline :: LocalTime,
    assignmentDescription :: Text,
    assignmentWeight :: Double,
    assignmentClassroom :: Int
  }
  deriving (Generic, FromJSON)