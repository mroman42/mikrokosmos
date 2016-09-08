module Format where

import System.Console.ANSI

promptColor :: Color
promptColor = Blue

nameColor :: Color
nameColor = Green

substColor :: Color
substColor = Red

formatFormula :: String
formatFormula = setSGRCode [SetConsoleIntensity NormalIntensity, SetColor Foreground Dull promptColor]

formatIntro :: String
formatIntro = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull promptColor]

formatPrompt :: String
formatPrompt = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid promptColor]

formatName :: String
formatName = setSGRCode [SetColor Foreground Dull nameColor]

formatSubs1 :: String
formatSubs1 = setSGRCode [SetConsoleIntensity BoldIntensity, SetColor Foreground Dull substColor]

formatSubs2 :: String
formatSubs2 = setSGRCode [SetColor Foreground Dull Yellow]

end :: String
end  = setSGRCode []
