module Format where

import System.Console.ANSI

promptColor :: Color
promptColor = Blue

formatFormula :: String
formatFormula = setSGRCode [SetColor Foreground Dull promptColor]

formatIntro :: String
formatIntro = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull promptColor]

formatPrompt :: String
formatPrompt = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid promptColor]

end :: String
end  = setSGRCode []
