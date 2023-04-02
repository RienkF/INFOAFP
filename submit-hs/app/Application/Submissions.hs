module Application.Submissions where

import Database.Model
import qualified Database.Submissions

getSubmissions :: IO [Submission]
getSubmissions = Database.Submissions.getSubmissions