module Application.Users where

import Api.Types (AddUserBody (userName, userType))
import Control.Monad.IO.Class
import Database.Model
import qualified Database.Users
import Servant

getUsers :: IO [User]
getUsers = Database.Users.getUsers

-- TODO: Add username validation
addUser :: AddUserBody -> Handler NoContent
addUser body = do
  liftIO $ Database.Users.addUser (userName body) (userType body)
  return NoContent