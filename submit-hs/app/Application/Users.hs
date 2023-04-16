module Application.Users where

import Control.Monad.IO.Class
import Database.Model
import qualified Database.Users
import Servant
import Api.Types.UserTypes

getUsers :: Maybe [Int] -> Handler [User]
getUsers idFilter = liftIO $ Database.Users.getUsers idFilter

-- TODO: Add username validation
addUser :: AddUserBody -> Handler (Maybe User)
addUser body = do
  liftIO $ Database.Users.addUser (userName body) (userType body)