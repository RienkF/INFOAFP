module Application.Users where

import Api.Types.UserTypes
import Control.Monad.IO.Class
import Database.Model
import qualified Database.Users
import Servant

getUsers :: Maybe [Int] -> Maybe [Int] -> Handler [User]
getUsers userIds classroomIds = liftIO $ Database.Users.getUsers userIds classroomIds

-- TODO: Add username validation
addUser :: AddUserBody -> Handler (Maybe User)
addUser body = do
  liftIO $ Database.Users.addUser (userName body) (userType body)