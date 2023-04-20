{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Types.GradingTypes where
import Database.Beam
import Data.Aeson
import Data.Text

--data GradingT f = Grading
--  { _gradingId :: C f Int32,
--    _gradingSubmission :: PrimaryKey SubmissionT f,
--    _gradingGrade :: C f Double,
--    -- User that did the review
--    _gradingUser :: PrimaryKey UserT f,
--    _gradingTimestamp :: C f LocalTime,
--    _gradingFeedback :: C f Text
--  }
--  deriving (Generic, Beamable)

data AddGradingBody = AddGradingBody
  { submissionId :: Int,
    grade :: Double,
    reviewerId :: Int,
    feedback :: Text
  }
  deriving (Generic, FromJSON)

-- TODO: DelAttemptBody = ..
