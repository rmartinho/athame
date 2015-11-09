module Athame.Canon (
    canonical,
    basic,
    equiv
) where

import Athame.Types

import Data.Function (on)
import Data.List (intersperse)

-- Class for testing algorithms for equivalence
-- Equivalence is tested by comparing canonical forms
-- Note that producing the same results is not enough: the cube must go through all the same changes
-- for equivalence. The one exception is basic moves immediately followed by
-- their inverse, sequences of the same three or four quarter-turns, and the inverses of half turns;
-- the latter are treated as clockwise half-turns, and the others are ignored for equivalence purposes.
class Eq m => Canon m where
    -- Convert a sequence of moves to its canonical form
    -- Canonical form properties:
    --  - uses quarter-turn metrics;
    --  - all half-turns are done as two clockwise quarter-turns;
    --  - there are no redundant series of turns of the same face.
    canonical  :: [Algorithm] -> [m] -> [Turn]

instance Canon QuarterTurn where
    canonical algs = canonical algs . map Quarter

instance Canon Turn where
    -- This is done with a multi-pass algorithm:
    -- 1. remove all double invertions (won't occur naturally, but can occur as a degenerate result of a transformation)
    -- 2. normalize all inverse half-turns into simple half-turns;
    -- 3. expand all half-turns;
    -- 4. expand all inverse quarter-turns into triple simple quarter-turns;
    -- 5. remove all quadruple quarter-turns;
    -- 6. replace all triple quarter-turns with the inverse.
    canonical _ =  normalizeTriples . removeQuadruples . expandInverses . expandHalves . normalizeInverseHalves . normalizeDoubleInverses
        where normalizeDoubleInverses = map normalizeDoubleInverse
              normalizeDoubleInverse (Inverse (Inverse t)) = normalizeDoubleInverse t
              normalizeDoubleInverse t = t
              normalizeInverseHalves = map normalizeInverseHalf
              normalizeInverseHalf (Inverse t@(Half _)) = t
              normalizeInverseHalf t = t
              expandHalves = concatMap expandHalf
              expandHalf (Half t) = [Quarter t, Quarter t]
              expandHalf t = [t]
              expandInverses [] = []
              expandInverses (Inverse t:ts) = t:t:t:expandInverses ts
              expandInverses (t:ts) = t:expandInverses ts
              removeQuadruples (t0:t1:t2:t3:ts) | t0 == t1 && t1 == t2 && t2 == t3 = removeQuadruples ts
                                                | otherwise = t0:removeQuadruples (t1:t2:t3:ts)
              removeQuadruples ts = ts
              normalizeTriples (t0:t1:t2:ts) | t0 == t1 && t1 == t2 = Inverse t0:normalizeTriples ts
                                             | otherwise = t0:normalizeTriples (t1:t2:ts)
              normalizeTriples ts = ts

instance Canon Move where
    -- Expand everything to sequences, then concatenate recursive expansions
    canonical algs = canonical algs . concatMap expand
        where expand (Elementary t) = [t]
              expand (Sequence ms) = concatMap expand ms
              expand m = expand (seq m)

              seq (Simultaneous t0 t1) = Sequence [Elementary t0, Elementary t1]
              seq m@(Elementary _) = Sequence [m]
              seq (Repeat ms n) = Sequence (concat $ replicate n ms)
              seq m@(Sequence _) = m
              seq (Group ms) = Sequence ms
              seq (Trigger ms) = Sequence ms
              seq (Conjugate m0 m1) = Sequence $ concat [m0, m1, [inverse (Sequence m0)]]
              seq (Commutator m0 m1) = Sequence $ concat [m0, m1, [inverse (Sequence m0)], [inverse (Sequence m1)]]
              seq (Infix m0 m1 n) = Sequence $ intersperse m1 $ replicate (n+1) m0
              seq (Named n) = Sequence $ moves (head $ filter sameName algs)
                where sameName a = name a == n

              inverse :: Move -> Move
              inverse (Elementary t) = Elementary (Inverse t)
              inverse (Sequence ms) = Sequence (reverse $ map inverse ms)
              inverse m = inverse (seq m)

-- Test if two algorithms are equivalent
equiv :: [Algorithm] -> Algorithm -> Algorithm -> Bool
equiv algs = (==) `on` (canonical algs . moves)

-- Like the canonical form, but with collapsed half-turns for basic display
basic :: [Algorithm] -> [Move] -> [Turn]
basic algs = collapseHalves . canonical algs
    where collapseHalves [] = []
          collapseHalves (Quarter q0:Quarter q1:qs) | q0 == q1 = Half q0:collapseHalves qs
                                                    | otherwise = Quarter q0:collapseHalves (Quarter q1:qs)
          collapseHalves (q:qs) = q:collapseHalves qs


