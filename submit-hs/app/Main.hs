module Main where

import Api.Server
import Control.Monad.IO.Class
import Database.Migrations.Migrate
import Database.SQLite.Simple
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  migrateDatabase
  run 3000 application