module Main where

import Api.Server
import Control.Monad.IO.Class
import Database.Db
import Database.SQLite.Simple
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  migrateDb
  run 3000 application
