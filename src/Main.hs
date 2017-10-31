module Main where

import Data.Char
import qualified Data.Map as M

import System.Directory
import System.Environment
import System.Exit

import Control.Applicative
import Control.Monad

import Project

data Verb
  = List
  | Start
  | Stop
  | Info
  | Edit
  | Create
  | Todos
  | Done
  | Activate
  | Suspend
  | Finish
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

getPmDir :: IO FilePath
getPmDir = do
  env <- M.fromList <$> getEnvironment
  home <- getHomeDirectory
  return $ M.findWithDefault (home ++ "/.pm/") "PM_HOME" env

getPmCur :: IO FilePath
getPmCur = do
  env <- M.fromList <$> getEnvironment
  home <- getHomeDirectory
  return $ M.findWithDefault (home ++ "/.pm_current") "PM_CURRENT" env

usage :: IO a
usage = putStrLn "usage: pm <verb> [project] (more help soon)" >> exitSuccess

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

getVerb :: IO Verb
getVerb = do
  args <- getArgs
  case args of
    [] -> usage
    (c:_) ->  let vs = capitalize c in
              if elem vs $ show <$> ([minBound..maxBound] :: [Verb])
                then return $ read vs
                else usage

main = do
  pmdir <- getPmDir
  pmcur <- getPmCur
  verb  <- getVerb
  print pmdir
  print pmcur
  print verb
  case verb of
    List -> listTasks
    _    -> putStrLn "Not Implemented Yet"

getTasks = return [Project "proj1" "blurb" "desc" "echo hello" "echo goodbye" []]


printTask task = putStrLn "Here's a task!"

listTasks = do
  tasks <- getTasks
  mapM_ print tasks
  mapM_ printTask tasks
