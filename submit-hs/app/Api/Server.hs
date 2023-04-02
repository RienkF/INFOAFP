module Api.Server where

import Api.Spec
import Application.Assignments
import Application.Classrooms
import Application.Gradings
import Application.Submissions
import Application.Users
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data
import Database.Attempts
import Servant

api :: Proxy Api.Spec.Spec
api = Proxy

server :: Server Api.Spec.Spec
server =
  liftIO getUsers
    :<|> liftIO getClassrooms
    :<|> liftIO getClassroomParticipants
    :<|> liftIO getAssignments
    :<|> liftIO getSubmissions
    :<|> liftIO getAttempts
    :<|> liftIO getGradings
    :<|> serveDirectoryFileServer "static/"

application :: Application
application = serve api server