module Main where

import Api
import Network.Wai.Handler.Warp

main :: IO ()
main = run 80 application
