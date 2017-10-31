module Project where

import Control.Monad
import Text.ParserCombinators.Poly.Parser
import Text.XML.HaXml.TypeMapping
import Text.XML.HaXml.XmlContent.Parser
import Text.XML.HaXml.XmlContent
import Text.XML.HaXml.Util
import Text.XML.HaXml hiding (element)
import Data.Maybe

data ProjectList = Projects (Maybe Int) [Project] deriving Show

instance HTypeable ProjectList where
  toHType (Projects current projects) = Defined "ProjectList" []
    [Constr "Projects" [] [toHType current, toHType projects]]

instance XmlContent ProjectList where

  parseContents = do
      let parseC e = return $ read <$> attrToText "current" (attrs e)
      e <- element ["projects"]
      interior e (Projects <$> parseC e <*> many parseContents)

  toContents p@(Projects c ps) =
    [mkAttrElemC "projects" currentattribute (toContents ps)]
    where
      currentattribute = case c of
        Nothing -> []
        Just c -> [mkAttr "current" (show c)]

data Project = Project {
                  getId :: Int,
                  name :: String,
                  blurb :: String,
                  desc :: String,
                  start :: String,
                  stop :: String,
                  todos :: [Todo]
                  } deriving (Show)

instance HTypeable Project where
  toHType (Project id n b d a z ts) = Defined "Project" []
    [Constr "Project" [] [toHType id, toHType n, toHType b, toHType d,
      toHType a, toHType z, toHType ts]]

instance XmlContent Project where
  parseContents = do
      let pId e = return . read . fromJust $ attrToText "id" $ attrs e
      let pName = inElement "name" text
      let pBlurb = inElement "blurb" text
      let pDesc = inElement "desc" text
      let pStart = inElement "start" text
      let pStop = inElement "stop" text
      let pTodos = inElement "todos" $ many (parseContents :: XMLParser Todo)
      e <- element ["project"]
      interior e (Project <$> pId e <*> pName <*> pBlurb 
                          <*> pDesc <*> pStart <*> pStop <*> pTodos)

  toContents (Project id n b d a z ts) =
    [mkAttrElemC "project" [mkAttr "id" (show id)] [
      mkElemC "name" (toText n),
      mkElemC "blurb" (toText b),
      mkElemC "desc" (toText d),
      mkElemC "start" (toText a),
      mkElemC "stop" (toText z),
      mkElemC "todos" (if null ts then [] else toContents ts)]]


data Todo = Todo Int String deriving Show

instance HTypeable Todo where
  toHType (Todo num task) = Defined "Todo" []
    [Constr "Todo" [] [toHType num, toHType task]]

instance XmlContent Todo where
  parseContents = do
      let pId e = return . read . fromJust $ attrToText "id" $ attrs e
      e <- element ["todo"]
      interior e (Todo <$> pId e <*> text)
  toContents (Todo id task) =
    [mkAttrElemC "todo" [mkAttr "id" (show id)] (toText task)]

-- helper functions for HaXml
mkAttrElemC :: String -> [Attribute] -> [Content ()] -> Content ()
mkAttrElemC x as cs = CElem (Elem (N x) as cs) ()

attrToText :: String -> [Attribute] -> Maybe String
attrToText n [] = Nothing
attrToText n as = foldl1 (<|>) attrText
       where attrText = map (fromAttrToStr n) as 

getProjects :: FilePath -> IO ProjectList
getProjects = fReadXml

writeProjects :: FilePath -> ProjectList -> IO ()
writeProjects = fpsWriteXml

printProjects :: ProjectList -> IO ()
printProjects (Projects cur ps) = do
  putStrLn "Here are your tasks:"
  putStrLn "------"
  mapM_ (printProject cur) ps
  putStrLn "-----"

-- display a single project as part of the list command (for info command, see
-- printProjectInfo)
printProject cur (Project id name blurb _ _ _ ts) = do
  let selector = case cur of
                    Nothing -> "    "
                    Just c  -> if c == id then "  + " else "    "
  let numts = show $ length ts
  putStrLn $ selector ++ "[" ++ name ++ "] (" ++ numts ++ " todos)"
  putStrLn $ "        " ++ blurb

printProjectInfo (Project id name blurb desc _ _ ts) = do
  putStrLn $ "    [" ++ name ++ "]"
  putStrLn "--------"
  putStrLn blurb
  putStrLn desc
  mapM_ showTodo ts

showTodo (Todo id desc) = putStrLn $ "[ ] (" ++ show id ++ "): " ++ desc

addProject :: ProjectList -> Project -> ProjectList
addProject (Projects id ps) p = Projects id (p:ps)

getProject pmfile n = do
  (Projects _ ps) <- getProjects pmfile
  let hasname p = name p == n
  return $ (listToMaybe . filter hasname) ps

updateProject:: FilePath -> Int -> Project -> IO ()
updateProject pmfile id newproj = do
  (Projects id' ps) <- getProjects pmfile
  let updateProjects' ps = case ps of
                            [] -> []
                            (p@(Project pid _ _ _ _ _ _):rps) -> (if pid == id then newproj else p) : updateProjects' rps
  writeProjects pmfile $ Projects id' $ updateProjects' ps

getProjectById (Projects _ ps) n =
  case filter ((== n) .getId) ps of
    [] -> Nothing
    [a] -> Just a
    (a:as) -> Just a

getCurrentProject p@(Projects id ps) = id >>= getProjectById p

getNextId pmfile = do
  let getNextId' n ps = if any ((==) n . getId) ps then getNextId' (n+1) ps else n
  (Projects _ ps) <- getProjects pmfile
  return $ getNextId' 0 ps
