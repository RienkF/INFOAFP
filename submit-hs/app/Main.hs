module Main where

import Api.Server
import Control.Monad.IO.Class
import Database.Db
import Database.Migrations.Migrate
import Database.SQLite.Simple

main :: IO ()
main = do
  migrateDb
  run 3000 application
