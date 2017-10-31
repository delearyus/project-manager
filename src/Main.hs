module Main where

import Data.Char
import qualified Data.Map as M

import System.Directory
import System.Environment
import System.Exit

import Control.Applicative
import Control.Monad

import Project
import TempFile

-- Verb type represents the commands that can be passed in 
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

data EditVerb
  = EditName
  | EditDesc
  | EditStart
  | EditStop
  | EditTodos
  deriving (Eq, Ord, Bounded, Enum, Show, Read)

-- The PM File is an XML document that contains information about current
-- projects
getPmFile :: IO FilePath
getPmFile = do
  env <- M.fromList <$> getEnvironment
  home <- getHomeDirectory
  return $ M.findWithDefault (home ++ "/.pm.xml") "PM_PROJECT_FILE" env

-- Print out usage and exit
usage :: IO a
usage = putStrLn "usage: pm <verb> [project] (more help soon)" >> exitSuccess

-- Look at the arguments and extract the Verb that is passed in
getVerb :: IO Verb
getVerb = do
  args <- getArgs
  case args of
    [] -> usage
    (c:_) ->  let vs = capitalize c in
              if elem vs $ show <$> ([minBound..maxBound] :: [Verb])
                then return $ read vs
                else usage

capitalize "" = ""
capitalize (c:cs) = toUpper c : cs

-- get all projects and display them
listProjects pf = getProjects pf >>= printProjects

main = do
  pmfile <- getPmFile
  verb  <- getVerb
  print pmfile
  print verb
  case verb of
    List -> listProjects pmfile
    Edit -> editProject pmfile
    Create -> createProject pmfile
    Stop -> stopCurrentProject pmfile
    Start -> startNewProject pmfile
    _    -> putStrLn "Not Implemented Yet"

stopCurrentProject pmfile = do
  p@(Projects cid ps) <- getProjects pmfile
  case getCurrentProject p of
    Nothing -> putStrLn "No active project"
    Just cp@(Project _ _ _ _ _ stop _) ->
      runString stop >> writeProjects pmfile (Projects Nothing ps)

startNewProject pmfile = do
  (v:pn:_) <- getArgs
  a <- getProject pmfile pn
  case a of
    Nothing -> putStrLn $ "Error: project " ++ pn ++ " does not exist."
    Just proj -> do
      (Projects _ ps) <- getProjects pmfile
      stopCurrentProject pmfile
      runString $ start proj
      writeProjects pmfile $ Projects (Just $ getId proj) ps

editProject fp = do
  ev <- getEditVerb
  (v1:v2:p:_) <- getArgs
  project <- getProject fp p
  case project of
    Nothing -> putStrLn "unknown project, see pm list"
    Just p@(Project id name blurb desc start stop todos)
      -> case ev of
          EditDesc -> do
            (newblurb:newdesc) <- lines <$> editString (blurb ++ "\n" ++ desc)
            let newproj = Project id name newblurb (unlines newdesc) start stop todos
            updateProject fp id newproj
          EditName -> do
            newname <- editString name
            let newproj = Project id newname blurb desc start stop todos
            updateProject fp id newproj
          EditStart -> do
            newstart <- editString start
            let newproj = Project id name blurb desc newstart stop todos
            updateProject fp id newproj
          EditStop -> do
            newstop <- editString stop
            let newproj = Project id name blurb desc start newstop todos
            updateProject fp id newproj
          EditTodos -> do
            putStrLn "not implemented yet lol that ones a little tricky"
            putStrLn "sorry"


getEditVerb :: IO EditVerb
getEditVerb = do
  args <- getArgs
  case args of
    (v1:v2:_) -> do
                  let vs = "Edit" ++ capitalize v2
                  if elem vs $ show <$> ([minBound..maxBound] :: [EditVerb])
                    then return $ read vs
                    else usage
    _ -> putStrLn "No Verb" >> usage

createProject pmfile = do
  projects <- getProjects pmfile
  (v1:n:b) <- getArgs
  newid <- getNextId pmfile
  let newproj = Project newid n (unwords b) "description goes here" "echo hello" "echo goodbye" []
  writeProjects pmfile (addProject projects newproj)
