module Block
  ( Block
  , textBlock
  , deductionBlock
  , box
  )
where 

import Data.Monoid

newtype Block = Block { getBlock :: [String] }
  deriving (Eq, Ord)

instance Monoid Block where
  mappend = joinBlocks
  mempty  = Block [[]]

instance Show Block where
  show = unlines . getBlock


hsepChar :: Char
hsepChar = '─'

spaceChar :: Char
spaceChar = ' '

height :: Block -> Int
height = length . getBlock

width :: Block -> Int
width (Block [])    = 0
width (Block (x:_)) = length x

-- | Horizontal join
joinBlocks :: Block -> Block -> Block
joinBlocks u@(Block a) v@(Block b) = Block $ zipWith (++) us vs
  where
    us = replicate (mh - uh) (replicate uw spaceChar) ++ a
    vs = replicate (mh - vh) (replicate vw spaceChar) ++ b
    uh = height u
    vh = height v
    mh = max uh vh
    uw = width u
    vw = width v

-- | Vertical join
stackBlocks :: String -> Block -> Block -> Block
stackBlocks label u v = Block $ getBlock ut ++ [hline] ++ getBlock vt
  where
    mw = max (width u) (width v)
    us = centerBlock mw u
    vs = centerBlock mw v
    hline = replicate mw hsepChar ++ label
    ut = us <> Block [replicate (length label) ' ']
    vt = vs <> Block [replicate (length label) ' ']


centerBlock :: Int -> Block -> Block
centerBlock n = Block . map (centerString n) . getBlock

centerString :: Int -> String -> String
centerString n s = centered
  where
    w = length s
    diff = n - w
    semicentered = replicate (diff `div` 2 + diff `mod` 2) ' ' ++ s
    centered = take n (semicentered ++ repeat ' ')

-- | Inserts a text into a text block
textBlock :: String -> Block
textBlock s = Block [centerString (length s) s]

-- | Draws a logical inference in a text block
deductionBlock :: Block -> String -> [Block] -> Block
deductionBlock inference label blocks = stackBlocks label top inference
  where
    top = if not (null blocks)
          then foldr1 (\x y -> x <> Block ["   "] <> y) blocks
          else Block [""]

-- | Draws a box around a text block
box :: Block -> Block
box c@(Block cm) = Block $
  ["╭" ++ replicate (width c) '─' ++ "╮"] ++
  map (" " ++) cm                  ++
  ["╰" ++ replicate (width c) '─' ++ "╯"]
