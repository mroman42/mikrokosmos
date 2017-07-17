{-|
Module: Format
Description: Formatting the output of the interpreter
License: GPL-3

This module controls the format of the text and expressions printed by the
interpreter. Uses ANSI escape sequences to color the terminal and mark text
as bold or italics. It also stores the texts showed by the interpreter.
-}

module Format
  (
  -- * Formatting
    formatFormula
  , formatIntro
  , formatLoading
  , formatPrompt
  , formatName
  , formatSubs1
  , formatSubs2
  , decolor
  , end

  -- * Interpreter texts
  , promptText
  , helpText
  , initialText
  , versionText
  )
where

import System.Console.ANSI
import Data.List
import Data.Monoid

-- Colors
-- | Prompt messages color
promptColor :: Color
promptColor = Blue

-- | Named variables color
nameColor :: Color
nameColor = Green

-- | Substitutions are marked with this color
substColor :: Color
substColor = Red

-- | To-be-substituted expressions are marked with this color
subst2Color :: Color
subst2Color = Yellow


-- Format sequences
-- | Sequence of characters that signals the format of a formula to the terminal.
formatFormula :: String
formatFormula = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull promptColor]

-- | Sequence of characters that signals the format of the introduction to the terminal.
formatIntro :: String
formatIntro = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull promptColor]

-- | Sequence of characters that signals the format of the loading of a module to the terminal.
formatLoading :: String
formatLoading = formatIntro

-- | Sequence of characters that signals the format of the prompt to the terminal.
formatPrompt :: String
formatPrompt = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid promptColor]

-- | Sequence of characters that signals the format of a name to the terminal.
formatName :: String
formatName = setSGRCode [SetColor Foreground Dull nameColor]

-- | Sequence of characters that signals the format of a substitution to the terminal.
formatSubs1 :: String
formatSubs1 = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull substColor]

-- | Sequence of characters that signals the format of a expression which will
--   be substituted in the next reduction step to the terminal.
formatSubs2 :: String
formatSubs2 = setSGRCode [SetColor Foreground Dull subst2Color]

-- | Sequence of characters that cleans all the format.
end :: String
end  = setSGRCode []


-- | Removes all the ocurrences of a string from the other
removeString :: String -> String -> String
removeString _ "" = ""
removeString t s@(c:sc)
  | t `isPrefixOf` s = removeString t (drop (length t) s)
  | otherwise = c : removeString t sc

-- | Removes all color from a string
decolor :: String -> String
decolor = appEndo $ mconcat $ map (Endo . removeString)
  [ formatSubs1
  , formatSubs2
  , formatFormula
  , formatIntro
  , formatName
  , formatPrompt
  , formatLoading
  , end
  ]



-- | Prompt line. It is shown when the interpreter asks the user
--   to introduce a new command.
promptText :: String
promptText = formatPrompt ++ "mikro> " ++ end

-- | Help line. It is shown when the user uses the :help command.
helpText :: String
helpText = unlines [
  formatFormula ++
  "Commands available from the prompt:",
  "\t<expression>\t evaluates the expression",
  "\t:quit       \t quits the interpreter",
  "\t:load <file>\t loads the given .mkr library or script",
  "\t:verbose    \t sets verbose mode on/off",
  "\t:help       \t shows this help"
  ++ end
  ]

-- | Initial text on the interpreter. It is shown at startup.
initialText :: String
initialText = unlines [
  formatIntro ++ "Welcome to the Mikrokosmos Lambda Interpreter!" ++ end,
  formatFormula ++ "Version " ++ version ++ ". GNU General Public License Version 3." ++ end
  ]

-- | Version complete text
versionText :: String
versionText = "Mikrokosmos, version " ++ version

-- | Version
version :: String
version = "Untyped λ 0.2.0"
