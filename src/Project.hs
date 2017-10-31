module Project where



data Project = Project {
                  name:: String,
                  blurb:: String,
                  desc:: String,
                  start:: String,
                  stop::String,
                  todos:: [Todo]
                  } deriving (Show)

data Todo = Todo Int String deriving Show
