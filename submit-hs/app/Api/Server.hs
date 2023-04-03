module Api.Server where

import Api.Spec
import Application.Assignments
import Application.Attempts
import Application.Classrooms
import Application.Gradings
import Application.Submissions
import Application.Users
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data
import Network.HTTP.Types
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import Servant.API (Delete, Get, NoContent, Post, Put)

api :: Proxy Api.Spec.Spec
api = Proxy

server :: Server Api.Spec.Spec
server =
  ( liftIO getUsers
      :<|> addUser
  )
    :<|> liftIO getClassrooms
    :<|> liftIO getClassroomParticipants
    :<|> liftIO getAssignments
    :<|> liftIO getSubmissions
    :<|> liftIO getAttempts
    :<|> liftIO getGradings
    :<|> serveDirectoryFileServer "static/"

application :: Application
application =
  let corsPolicy =
        simpleCorsResourcePolicy
          { corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions],
            corsRequestHeaders = [hAuthorization, hContentType, hUserAgent, hAccept]
          }
   in cors (const $ Just corsPolicy) $ serve api server