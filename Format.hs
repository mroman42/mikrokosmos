module Format where

import System.Console.ANSI

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
formatSubs2 = setSGRCode [SetColor Foreground Dull Yellow]

-- | Sequence of characters that cleans all the format.
end :: String
end  = setSGRCode []
