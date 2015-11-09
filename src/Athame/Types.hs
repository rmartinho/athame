module Athame.Types (
    QuarterTurn(..),
    Turn(..),
    Move(..),
    Algorithm(..)
) where

-- Elementary quarter-turns
data QuarterTurn = F | B | U | D | L | R       -- face turns
                 | M | E | S                   -- slice turns
                 | X | Y | Z                   -- rotations
                 | Fw | Bw | Uw | Dw | Lw | Rw -- double-layer turns
                 deriving Eq

-- Any turns: quarter, half, or inverse
data Turn = Quarter QuarterTurn
          | Half QuarterTurn
          | Inverse Turn
          deriving Eq

-- Any kind of move descriptor (i.e. more complex moves allowed as a single unit)
data Move = Elementary Turn
          | Simultaneous Turn Turn
          | Sequence [Move]
          | Group [Move]
          | Repeat [Move] Int
          | Trigger [Move]
          | Conjugate [Move] [Move]
          | Commutator [Move] [Move]
          | Infix Move Move Int
          | Named String
          deriving Eq

data Algorithm = Algorithm { name :: String, moves :: [Move] }
                 deriving Eq
