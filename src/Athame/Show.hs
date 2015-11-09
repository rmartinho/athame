module Athame.Show() where

import Athame.Types

import Data.List (intersperse)

instance Show QuarterTurn where
    show F = "F"
    show B = "B"
    show U = "U"
    show D = "D"
    show L = "L"
    show R = "R"
    show M = "M"
    show E = "E"
    show S = "S"
    show X = "x"
    show Y = "y"
    show Z = "z"
    show Fw = "f"
    show Bw = "b"
    show Uw = "u"
    show Dw = "d"
    show Lw = "l"
    show Rw = "r"

instance Show Turn where
    show (Quarter q) = show q
    show (Half q) = shows q "2"
    show (Inverse t) = shows t "'"
    showList [] = ('O':)
    showList ts = showsSeparatedBy " " ts

instance Show Move where
    show m = show' m ""
        where show' (Elementary t) = shows t
              show' (Simultaneous t0 t1) = showSurrounded "()" $ shows t0 . showString "/" . shows t1
              show' (Sequence ms) = showList ms
              show' (Group ms) = showGroup "()" ms
              show' (Repeat ms n) = showGroup "()" ms . shows n
              show' (Trigger ms) = showGroup "<>" ms
              show' (Conjugate m0 m1) = showSurrounded "[]" $ showList m0 . showString ": " . showList m1
              show' (Commutator m0 m1) = showSurrounded "[]" $ showList m0 . showString ", " . showList m1
              show' (Infix m0 m1 n) = showSurrounded "[]" $ show' m0 . showString (replicate n '*') . show' m1
              show' (Named s) = showSurrounded "{}" $ showString s
              showGroup _ [] = ('O':)
              showGroup _ [m] = show' m
              showGroup p ms = showSurrounded p $ shows ms
              showSurrounded [o, c] s = showString [o] . s . showString [c]
    showList [] = ('O':)
    showList ms = showsSeparatedBy " " ms


instance Show Algorithm where
    show (Algorithm n ms) = show' n ms ""
        where show' n ms = showString n . showString ": " . shows ms

-- Util stuff
showsSeparatedBy :: Show a => String -> [a] -> ShowS
showsSeparatedBy s = foldr (.) id . intersperse (s++) . map shows

