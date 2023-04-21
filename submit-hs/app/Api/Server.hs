{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Api.Server where

import Api.Spec
import Application.Assignments (addAssignment, getAssignments, deleteAssignment)
import Application.Attempts
import Application.Classrooms
import Application.Gradings
import Application.Submissions
import Application.Users
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Data
import Data.String
import Data.Text hiding (break, dropWhile, foldr, splitOn)
import Data.Text.Internal
import Network.HTTP.Types
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import Servant.API (Delete, Get, NoContent, Post, Put)
import Text.Read (readMaybe)

api :: Proxy Api.Spec.Spec
api = Proxy

server :: Server Api.Spec.Spec
server =
  ( getUsers :<|> addUser :<|> delUserById
  )
    :<|> (getClassrooms :<|> addClassroom :<|> deleteClassroom)
    :<|> (getClassroomParticipants :<|> addClassroomParticipant :<|> deleteClassroomParticipant)
    :<|> (getAssignments :<|> addAssignment :<|> deleteAssignment)
    :<|> (getSubmissions :<|> addSubmission :<|> deleteSubmission)
    :<|> (getAttempts :<|> addAttempt :<|> deleteAttempt)
    :<|> (getGradings :<|> addGrading :<|> deleteGrading)
    :<|> serveDirectoryFileServer "static/"

application :: Application
application =
  let corsPolicy =
        simpleCorsResourcePolicy
          { corsMethods = [methodGet, methodPost, methodPut, methodDelete, methodOptions],
            corsRequestHeaders = [hAuthorization, hContentType]
          }
   in cors (const $ Just corsPolicy) $ serve api server

-- Allow lists in query string by splitting on ,
instance FromHttpApiData [Int] where
  parseUrlPiece :: Text -> Either Text [Int]
  parseUrlPiece text = case parseAll $ splitOn ',' $ unpack text of
    Just val -> Right val
    Nothing -> Left $ fromString "error"

-- Modified https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
splitOn :: Char -> String -> [String]
splitOn c s = case dropWhile (== c) s of
  "" -> []
  s' -> w : splitOn c s''
    where
      (w, s'') = break (== c) s'

-- Parse list of strings to ints if all of them are ints
parseAll :: [String] -> Maybe [Int]
parseAll = foldr (\x y -> readMaybe x >>= \z -> y >>= \y' -> Just (z : y')) (Just [])
