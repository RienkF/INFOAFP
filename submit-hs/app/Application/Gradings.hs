module Application.Gradings where

import qualified Database.Gradings
import Database.Model
import Servant
import Database.Beam

getGradings :: Handler [Grading]
getGradings = liftIO Database.Gradings.getGradings
