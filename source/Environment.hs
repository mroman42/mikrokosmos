{-|
Module: Environment
Description: Internal state and environment of the interpreter
License: GPL-3

This module contains all the auxiliary logic necessary to represent the internal
state of the interpreter.
-}
module Environment
  (
  -- * Environment
    Environment
  , context
  , defaultEnv

  -- * Reading the environment
  , getVerbose
  , getColor
  , getExpressionName
  
  -- * Modifying the environment
  , addBind
  , changeColor
  , changeVerbose

  -- * Filename
  , Filename
  )
where

import           Data.List
import           MultiBimap
import           Lambda

-- | A filename is a string containing the directory path and
-- the real name of the file.
type Filename = String

data Environment = Environment
  { context :: Context
  , loadedFiles :: [Filename]
  , verbose :: Bool
  , color :: Bool
  }

-- | Default environment for the interpreter.
defaultEnv :: Environment
defaultEnv = Environment
  { context     = emptyContext
  , loadedFiles = []
  , verbose     = False
  , color       = True
  }


addBind :: Environment -> String -> Exp -> Environment
addBind env s e = env {context = MultiBimap.insert e s (context env)}

-- | Gets the color configuration
getColor :: Environment -> Bool
getColor = color

-- | Gets the verbose configuration
getVerbose :: Environment -> Bool
getVerbose = verbose

-- | Sets the verbose configuration on/off.
changeVerbose :: Environment -> Environment
changeVerbose options = options {verbose = not $ verbose options}

-- | Sets the color configuration on/off
changeColor :: Environment -> Environment
changeColor options = options {color = not $ color options}

-- | Given an expression, returns its name if it is bounded to any.
getExpressionName :: Environment -> Exp -> Maybe String
getExpressionName environment expr = case MultiBimap.lookup expr (context environment) of
  [] -> Nothing
  xs -> Just $ intercalate ", " xs



-- | A context is an application between expressions and the names
-- they may have.
type Context  = MultiBimap Exp String

-- | Empty context without any bindings
emptyContext :: Context
emptyContext = MultiBimap.empty
