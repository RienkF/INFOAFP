module Main where

import Api
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class
import Database.SQLite.Simple
import Database.Db

main :: IO ()
main = do
    migrateDb
    run 3000 application