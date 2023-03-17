module Main where

import Api
import Network.Wai.Handler.Warp
import Database (submitDb)

main :: IO ()
main = run 80 application