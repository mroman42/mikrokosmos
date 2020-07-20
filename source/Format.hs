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
  , formatType
  , decolor
  , end

  -- * Interpreter texts
  , promptText
  , helpText
  , initialText
  , versionText
  , restartText
  , errorNonTypeableText
  , errorTypeConstructors
  , errorUndefinedText
  , errorUnknownCommand
  , errorNotFoundText
  )
where

import System.Console.ANSI
import Data.List
import Data.Monoid

-- | Colors used on the prompt.
promptColor, nameColor, substColor, subst2Color, typeColor, errorColor :: Color
promptColor = Blue
nameColor = Green
substColor = Cyan
subst2Color = Cyan
typeColor = Yellow
errorColor = Red

-- Format sequences
-- | Sequence of characters that signals the format of a formula to the terminal.
formatFormula, formatIntro, formatLoading, formatPrompt, formatName  :: String
formatSubs1, formatSubs2, formatType, formatError :: String
formatFormula = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull promptColor]
formatIntro   = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull promptColor]
formatLoading = formatIntro
formatPrompt  = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid promptColor]
formatName    = setSGRCode [SetColor Foreground Dull nameColor]
formatSubs1   = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull substColor]
formatSubs2   = setSGRCode [SetConsoleIntensity FaintIntensity, SetColor Foreground Dull subst2Color]
formatType    = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull typeColor]
formatError   = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull errorColor]

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
  , formatType
  , formatPrompt
  , formatLoading
  , formatError
  , end
  ]


-- | Unknown command error message
errorUnknownCommand :: String
errorUnknownCommand =
  formatError ++
  "Error: parse error, unknown command"
  ++ end


-- | Non typeable expression error message.
errorNonTypeableText :: String
errorNonTypeableText =
  formatError ++
  "Error: non typeable expression"
  ++ end

-- | Type constructors on untyped lambda calculus error message.
errorTypeConstructors :: String
errorTypeConstructors =
  formatError ++
  "Error: this expression uses type constructors. You may want to activate ':types on'."
  ++ end

-- | Undefined expression error message.
errorUndefinedText :: String
errorUndefinedText =
  formatError ++
  "Error: undefined terms on the lambda expression"
  ++ end

errorNotFoundText :: String
errorNotFoundText =
  formatError ++
  "Error: module or dependencies cannot be found"
  ++ end

restartText :: String
restartText = "Mikrokosmos context has been cleaned up"

-- | Prompt line. It is shown when the interpreter asks the user
--   to introduce a new command.
promptText :: String
promptText = formatPrompt ++ "mikro> " ++ end

-- | Help line. It is shown when the user uses the :help command.
helpText :: String
helpText = unlines [
  formatFormula ++
  "Commands available from the prompt:",
  "\t<expression>\t\t evaluates the expression",
  "\t:quit       \t\t quits the interpreter",
  "\t:restart    \t\t restarts the interpreter",
  "\t:load <file>\t\t loads the given .mkr library or script",
  "\t:verbose <on/off> \t sets verbose mode on/off",
  "\t:color <on/off> \t sets color mode on/off",
  "\t:ski <on/off> \t\t sets ski mode on/off",
  "\t:types <on/off> \t untyped/simply typed lambda calculus",
  "\t:strategy <full|cbv>  \t sets evaluation strategy to full beta/call by value", 
  "\t:help       \t\t shows this help"
  ++ end
  ]

-- | Initial text on the interpreter. It is shown at startup.
initialText :: String
initialText = unlines [
  formatIntro ++ "Welcome to the Mikrokosmos Lambda Interpreter!" ++ end,
  formatFormula ++ "Version " ++ version ++ ". GNU General Public License Version 3." ++ end
  ]

-- | Version, complete text
versionText :: String
versionText = "Mikrokosmos, version " ++ version

-- | Version
version :: String
version = "0.8.0"
