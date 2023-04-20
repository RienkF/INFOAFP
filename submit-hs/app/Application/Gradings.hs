module Application.Gradings where

import Database.Beam
import qualified Database.Gradings
import Database.Model
import Servant

getGradings :: Maybe [Int] -> Maybe [Int] -> Handler [Grading]
getGradings gradingIds submissionIds = liftIO $ Database.Gradings.getGradings gradingIds submissionIds
