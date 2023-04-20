{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Api.Types.GradingTypes where
import Database.Beam
import Data.Aeson
import Data.Text

data AddGradingBody = AddGradingBody
  { submissionId :: Int,
    grade :: Double,
    reviewerId :: Int,
    feedback :: Text
  }
  deriving (Generic, FromJSON)

