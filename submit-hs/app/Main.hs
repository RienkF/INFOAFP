module Main where

import Api
import Network.Wai.Handler.Warp
import Database (submitDb, getSubmit, migrateDB)
import Control.Monad.IO.Class
import Database.SQLite.Simple

main :: IO ()
-- main = run 80 application
main = do
    conn <- open "database.db"
    migrateDB conn
    return ()