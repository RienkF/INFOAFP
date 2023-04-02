module Application.Users where

import Database.Model
import qualified Database.Users

getUsers :: IO [User]
getUsers = Database.Users.getUsers