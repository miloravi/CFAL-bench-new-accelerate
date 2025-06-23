{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Quickhull where

import Data.Array.Accelerate
import qualified Data.Array.Accelerate.Unsafe as Unsafe
import qualified Prelude

-- Imports for recurive task-parallel implementation. Not used in the fully flattened implementation.
import Control.Concurrent
import Control.Exception
import System.IO.Unsafe

type Point = (Double, Double)
type Line = (Point, Point)
type Label = Int

-- Flattened state
type StateFlat =
  ( Vector (Point, Label) -- L, the points that should still be handled, combined with the index of their segment
  , Vector Point -- R, the points that are definitely on the convex hull.
  -- Label i corresponds with the points between R[i] and R[i+1]
  )

-- Non-flattened state
type State = (Vector Point, Scalar Line)

-- Fully flattened
quickhull1 :: Acc (Vector Point) -> Acc (Vector Point)
quickhull1
  = asnd
  -- While there are undecided points,
  . awhile
      (\(T2 points _) -> unit $ size points /= 0)
      -- Add more points to the hull
      stepFlat
  -- After creating an initial state, with the input split in two segments
  . initialize

-- Mostly flattened. In initializeSplit, we split the instance in two,
-- which we then both solve (in parallel). Finally we concatenate these two arrays.
quickhull2 :: Acc (Vector Point) -> Acc (Vector Point)
quickhull2 input = concatFinal above' below'
  where
    T2 above below = initializeSplit input
    T2 _ above' = awhile (\(T2 points _) -> unit $ size points /= 0) stepFlat $ stepToFlat above
    T2 _ below' = awhile (\(T2 points _) -> unit $ size points /= 0) stepFlat $ stepToFlat below

quickhullRecursiveThenFlatten
  :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> (a -> b))
  -> Int
  -> Vector Point
  -> Vector Point
quickhullRecursiveThenFlatten runN' =
  -- Make sure that evaluating 'quickhullRecursive runN' to WHNF, also causes
  -- all Accelerate functions to be compiled.
  initialize' `Prelude.seq` step' `Prelude.seq` concat1 `Prelude.seq` concat2 `Prelude.seq` flat `Prelude.seq`
    \maxDepth input -> unsafePerformIO $ do
      let (a, b) = initialize' input
      (a', b') <- goParallel maxDepth a b
      Prelude.return $ concat2 (a', b')
  where
    initialize' = runN' initializeSplit
    step' = runN' step
    concat1 = runN' $ uncurry concatWithoutOverlap
    concat2 = runN' $ uncurry concatFinal
    flat = runN' $ asnd . awhile (\(T2 points _) -> unit $ size points /= 0) stepFlat . stepToFlat
    
    goParallel :: Int -> State -> State -> Prelude.IO (Vector Point, Vector Point)
    goParallel maxDepth state1 state2 = do
      mvar <- newEmptyMVar
      _ <- forkIO $ do
        result1 <- go maxDepth state1 Prelude.>>= evaluate
        putMVar mvar result1
      result2 <- go maxDepth state2 Prelude.>>= evaluate
      result1 <- takeMVar mvar
      Prelude.return (result1, result2)

    go :: Int -> State -> Prelude.IO (Vector Point)
    go maxDepth state@(points, line)
      -- Base case: no more points
      | arrayShape points Prelude.== (Z :. 0)
      , (p1, p2) <- indexArray line Z
      = Prelude.return $ fromList (Z :. 2) [p1, p2]
      | maxDepth Prelude.== 0
      = Prelude.return $ flat state
      | (a, b) <- step' state = do
        (a', b') <- goParallel (maxDepth - 1) a b
        Prelude.return $ concat1 (a', b')

measureRecursionDepth
  :: (forall a b. (Arrays a, Arrays b) => (Acc a -> Acc b) -> (a -> b))
  -> Vector Point
  -> Int
measureRecursionDepth runN' = go . initialize'
  where
    go :: StateFlat -> Int
    go state
      | indexArray (condition' state) Z = 1 + go (step' state)
      | otherwise = 0

    initialize' = runN' initialize
    step' = runN' stepFlat
    condition' = runN' $ \(T2 points _) -> unit $ size points /= 0

concatWithoutOverlap :: Acc (Vector Point) -> Acc (Vector Point) -> Acc (Vector Point)
concatWithoutOverlap a b = generate (I1 $ size a + size b - 1) $ \(I1 idx) ->
  ifThenElse (idx < size a) (a !! idx) (b !! (idx - size a + 1))

concatFinal :: Acc (Vector Point) -> Acc (Vector Point) -> Acc (Vector Point)
concatFinal a b = generate (I1 $ size a + size b - 2) $ \(I1 idx) ->
  ifThenElse (idx < size a) (a !! idx) (b !! (idx - size a + 1))

initialize :: Acc (Vector Point) -> Acc StateFlat
initialize points = T2 newPoints hull
  where
    -- Find the leftmost and rightmost points.
    -- If there are multiple leftmost or rightmost points,
    -- we choose the one with the minimum or maximum y coordinate.
    line@(T2 leftMost rightMost)
      = the $ fold
        (\(T2 min1 max1) (T2 min2 max2) -> T2 (minPoint min1 min2) (maxPoint max1 max2))
        ((T2 (T2 (1.0/0.0) (1.0/0.0)) (T2 (-1.0/0.0) (-1.0/0.0))))
      $ map (\p -> T2 p p) points
    -- Note that this can be done with two folds (one for leftmost and one for rightmost),
    -- but we fused them manually.
  
    -- Partition the array in two segments: the points above the line, and the points below the line.
    -- This is implicit in the description in the paper.
    -- We drop all the points on the line, as they are definitely not on the convex hull.
    -- Preserving these elements may give problems, for instance if all points are on one line.
    -- By doing so, we also drop 'leftMost' and 'rightMost'.

    -- Partitioning happens similar to a filter (compaction).
    -- We use a scan to compute destination indices within a segment,
    -- and then offset the second segment by the size of the first segment.
    offsets :: Acc (Vector (Int, Int))
    counts :: Acc (Scalar (Int, Int))
    T2 offsets counts
      = scanl' (\(T2 l1 l2) (T2 r1 r2) -> T2 (l1 + r1) (l2 + r2)) (T2 0 0)
      $ map f points
      where
        -- Returns (1, 0) if the point is in the upper segment,
        -- (0, 1) if it is in the lower segment,
        -- or (0, 0) otherwise.
        f :: Exp Point -> Exp (Int, Int)
        f p
          = cond (p == leftMost ||! p == rightMost) (T2 0 0)
          $ cond (d > 0.0) (T2 1 0)
          $ cond (d < 0.0) (T2 0 1)
            (T2 0 0)
          where d = nonNormalizedDistance line p

    T2 countUpper countLower = the counts

    empty :: Acc (Vector (Point, Label))
    empty = fill (I1 $ countUpper + countLower) Unsafe.undef

    -- Perform the actual permutation
    newPoints :: Acc (Vector (Point, Label))
    newPoints
      = permuteUnique' empty
      $ zipWith f points offsets
      where
        f :: Exp Point -> Exp (Int, Int) -> Exp (Maybe (DIM1, (Point, Label)))
        f p (T2 idxUpper idxLower)
          = cond (p == leftMost ||! p == rightMost) Nothing_
          $ cond (d > 0.0) (Just_ $ T2 (I1 idxUpper) (T2 p 0))
          $ cond (d < 0.0) (Just_ $ T2 (I1 $ countUpper + idxLower) (T2 p 1))
            Nothing_
          where
            d = nonNormalizedDistance line p

    -- Initial hull consists of only leftMost and rightMost
    hull :: Acc (Vector Point)
    hull = generate (Z_ ::. 2) (\(I1 idx) -> idx == 0 ? (leftMost, rightMost))

-- Variant of initialize, that does not flatten the output.
-- Instead, it returns two StateFlats: one for the part of the input above the
-- line, one for the part below the line.
initializeSplit :: Acc (Vector Point) -> Acc (State, State)
initializeSplit points = T2
  (T2 above $ unit $ T2 leftMost rightMost)
  (T2 below $ unit $ T2 rightMost leftMost)
  where
    -- Find the leftmost and rightmost points.
    -- If there are multiple leftmost or rightmost points,
    -- we choose the one with the minimum or maximum y coordinate.
    line@(T2 leftMost rightMost)
      = the $ fold
        (\(T2 min1 max1) (T2 min2 max2) -> T2 (minPoint min1 min2) (maxPoint max1 max2))
        ((T2 (T2 (1.0/0.0) (1.0/0.0)) (T2 (-1.0/0.0) (-1.0/0.0))))
      $ map (\p -> T2 p p) points

    above = afst
      $ filter (\p -> p /= leftMost &&! p /= rightMost &&! nonNormalizedDistance line p > 0.0) points
    below = afst
      $ filter (\p -> p /= leftMost &&! p /= rightMost &&! nonNormalizedDistance line p < 0.0) points

stepFlat :: Acc StateFlat -> Acc StateFlat
stepFlat (T2 pointsWithLabels hull) = T2 newPoints newHull
  where
    (points, labels) = unzip pointsWithLabels

    -- Use a segmented scan to compute the furthest point to the line of each segment.
    -- Only the value at the last position of a segment is accurate after this scan. 
    furthest :: Acc (Vector Point)
    furthest
      = map snd -- Drop the distance, only keep the point
      $ segmentedScanl1 max labels
      $ map (\(T2 p segment) -> T2 (distance segment p) p) pointsWithLabels

    distance :: Exp Label -> Exp Point -> Exp Double
    distance label point = nonNormalizedDistance (T2 a b) point
      where
        a = hull !! label
        b = hull !! (label == size hull - 1 ? (0, label + 1))

    -- Store furthest point per segment
    furthestPerLabel :: Acc (Vector Point)
    furthestPerLabel = permuteUnique'
      -- Use -Infinity as default, to detect empty segments
      -- (parts of the hull where no undecided points remain)
      (generate (shape hull) $ const $ noPoint)
      -- Only write the value if it is the last of a segment
      $ imap
        (\(I1 idx) point ->
          (idx == size points - 1 || labels !! idx /= labels !! (idx + 1))
          ? (Just_ $ T2 (I1 $ labels !! idx) point, Nothing_)  
        )
        furthest

    noPoint = T2 (-1.0/0.0) (-1.0/0.0)
  
    -- Mapping from the old indices in the hull to the new indices.
    hullNewIndices :: Acc (Vector Int)
    hullNewSize :: Acc (Scalar Int)
    T2 hullNewIndices hullNewSize = scanl' (+) 0 $ map (\p -> p == noPoint ? (1, 2)) furthestPerLabel

    -- Add furthest points to the hull
    newHull :: Acc (Vector Point)
    newHull = permuteUnique'
      (permuteUnique'
        (generate (I1 $ the hullNewSize) $ const Unsafe.undef)
        $ imap
          (\idx point -> Just_ $ T2 (I1 $ hullNewIndices ! idx) point)
          hull
      )
      $ imap
        (\idx point -> cond (furthestPerLabel ! idx == noPoint) Nothing_
          $ Just_ $ T2 (I1 $ (hullNewIndices ! idx) + 1) point)
        furthestPerLabel

    -- Filter and reorder the remaining points.
    -- If the corresponding point in the hull of a given segment are A and B,
    -- and the furthest point is F, then we need to first store all the
    -- elements left to the line from A to F and then the points to the left
    -- of the line from F to B. Points within the triangle formed by A, B and F
    -- are dropped.

    -- Characterize whether this point should is left to AF (1), left to
    -- (FB) (2) or neither (0).
    groups :: Acc (Vector Int8)
    groups = map f pointsWithLabels
      where
        f :: Exp (Point, Label) -> Exp Int8
        f (T2 p label)
          = cond (p == furthestPoint) 0
          $ cond (nonNormalizedDistance (T2 a furthestPoint) p > 0.0) 1
          $ cond (nonNormalizedDistance (T2 furthestPoint b) p > 0.0) 2
            0
          where
            a = hull !! label
            b = hull !! (label == size hull - 1 ? (0, label + 1))
            furthestPoint = furthestPerLabel !! label

    -- Compute the destinations for all points.
    -- First compute the local offsets, within a group, within a segment.
    -- Note that this is an inclusive scan, so the offsets are off by one.
    localOffsets :: Acc (Vector (Int, Int))
    localOffsets
      = segmentedScanl1 (\(T2 a1 a2) (T2 b1 b2) -> T2 (a1 + b1) (a2 + b2)) labels
      $ map (\g -> cond (g == 1) (T2 1 0) $ cond (g == 2) (T2 0 1) (T2 0 0)) groups

    segmentSizes :: Acc (Vector (Int, Int))
    segmentSizes = permuteUnique'
      (generate (shape hull) $ const $ T2 0 0)
      $ imap
        (\(I1 idx) value ->
          (idx == size points - 1 || labels !! idx /= labels !! (idx + 1))
          ? (Just_ $ T2 (I1 $ labels !! idx) value, Nothing_)  
        )
        localOffsets
    
    segmentOffsets :: Acc (Vector Int)
    newSize :: Acc (Scalar Int)
    T2 segmentOffsets newSize = scanl' (+) 0 $ map (\(T2 size1 size2) -> size1 + size2) segmentSizes

    destination :: Elt a => Exp DIM1 -> Exp a -> Exp (Maybe (DIM1, a))
    destination idx value
      = cond (g == 1) (Just_ $ T2 (I1 $ segmentOffset + leftOffset - 1) value)
      $ cond (g == 2) (Just_ $ T2 (I1 $ segmentOffset + leftSize + rightOffset - 1) value)
      Nothing_
      where
        g = groups ! idx
        label = labels ! idx
        T2 leftOffset rightOffset = localOffsets ! idx
        segmentOffset = segmentOffsets !! label
        T2 leftSize _ = segmentSizes !! label

    setNewLabel :: Exp (Point, Label) -> Exp Int8 -> Exp (Point, Label)
    setNewLabel (T2 point label) g = T2 point $ (hullNewIndices !! label) + fromIntegral g - 1

    newPoints :: Acc (Vector (Point, Label))
    newPoints
      = permuteUnique' (generate (I1 $ the newSize) $ const Unsafe.undef)
      $ imap destination
      $ zipWith setNewLabel pointsWithLabels groups

stepToFlat :: Acc State -> Acc StateFlat
stepToFlat (T2 points line') = T2 newPoints newHull
  where
    line@(T2 p1 p2) = the line'

    -- Use a segmented scan to compute the furthest point to the line of each segment.
    -- Only the value at the last position of a segment is accurate after this scan. 
    furthest :: Exp Point
    furthest
      = snd -- Drop the distance, only keep the point
      $ the
      $ fold max (T2 0 noPoint)
      $ map (\p -> T2 (nonNormalizedDistance line p) p) points

    noPoint = T2 (1.0/0.0) (1.0/0.0)
  
    hullNewSize :: Exp Int
    hullNewSize = ifThenElse (furthest == noPoint) 2 3

    newHull :: Acc (Vector Point)
    newHull = generate (Z_ ::. hullNewSize) $ \(I1 idx) ->
      ifThenElse (idx == 0) p1
      $ ifThenElse (idx == hullNewSize - 1) p2
      furthest

    newLabels :: Acc (Vector Int)
    newLabels = map f points
      where
        f :: Exp Point -> Exp Int
        f p
          = cond (p == furthest) 2
          $ cond (nonNormalizedDistance (T2 p1 furthest) p > 0.0) 0
          $ cond (nonNormalizedDistance (T2 furthest p2) p > 0.0) 1
            2

    -- Compute the destinations for all points.
    -- First compute the local offsets, within a group, within a segment.
    -- Note that this is an inclusive scan, so the offsets are off by one.
    offsets :: Acc (Vector (Int, Int))
    segmentSizes :: Acc (Scalar (Int, Int))
    T2 offsets segmentSizes
      = scanl' (\(T2 a1 a2) (T2 b1 b2) -> T2 (a1 + b1) (a2 + b2)) (T2 0 0)
      $ map (\g -> cond (g == 0) (T2 1 0) $ cond (g == 1) (T2 0 1) (T2 0 0)) newLabels

    T2 leftSize rightSize = the segmentSizes

    destination :: Elt a => Exp DIM1 -> Exp a -> Exp (Maybe (DIM1, a))
    destination idx value
      = cond (g == 0) (Just_ $ T2 (I1 $ leftOffset) value)
      $ cond (g == 1) (Just_ $ T2 (I1 $ leftSize + rightOffset) value)
      Nothing_
      where
        g = newLabels ! idx
        T2 leftOffset rightOffset = offsets ! idx

    newPoints :: Acc (Vector (Point, Label))
    newPoints
      = permuteUnique' (generate (I1 $ leftSize + rightSize) $ const Unsafe.undef)
      $ imap destination $ zip points newLabels

step :: Acc State -> Acc (State, State)
step (T2 points line') = T2 (T2 left (unit $ T2 p1 furthest)) (T2 right (unit $ T2 furthest p2))
  where
    line@(T2 p1 p2) = the line'

    -- Use a segmented scan to compute the furthest point to the line of each segment.
    -- Only the value at the last position of a segment is accurate after this scan. 
    furthest :: Exp Point
    furthest
      = snd -- Drop the distance, only keep the point
      $ the
      $ fold max (T2 0 noPoint)
      $ map (\p -> T2 (nonNormalizedDistance line p) p) points

    noPoint = T2 (1.0/0.0) (1.0/0.0)

    T2 left _ = filter (\p -> p /= furthest && nonNormalizedDistance (T2 p1 furthest) p > 0.0) points
    T2 right _ = filter (\p -> p /= furthest && nonNormalizedDistance (T2 furthest p2) p > 0.0) points

-- Labeled inclusive scan.
segmentedScanl1 :: Elt a => (Exp a -> Exp a -> Exp a) -> Acc (Vector Label) -> Acc (Vector a) -> Acc (Vector a)
segmentedScanl1 f labels vector
  -- Drop the flags
  = map snd
  -- Lift 'f' to a segmented operator
  $ scanl1 (segmented f)
  -- Pair the values with flags denoting whether a segment starts there
  $ imap (\(I1 idx) a -> T2 (idx == 0 || labels !! idx /= labels !! (idx - 1)) a) vector

segmented :: Elt a => (Exp a -> Exp a -> Exp a) -> Exp (Bool, a) -> Exp (Bool, a) -> Exp (Bool, a)
segmented f (T2 aF aV) (T2 bF bV) = T2 (aF || bF) (bF ? (bV, f aV bV))

-- Computes the distance of a point to a line, which is off by a factor depending on the line.
nonNormalizedDistance :: Exp Line -> Exp Point -> Exp Double
nonNormalizedDistance (T2 (T2 x1 y1) (T2 x2 y2)) (T2 x y) = nx * x + ny * y - c
  where
    nx = y1 - y2
    ny = x2 - x1
    c  = nx * x1 + ny * y1

minPoint :: Exp Point -> Exp Point -> Exp Point
minPoint (T2 x1 y1) (T2 x2 y2) =
  cond (x1 < x2 ||! (x1 == x2 &&! y1 < y2)) (T2 x1 y1) (T2 x2 y2)

maxPoint :: Exp Point -> Exp Point -> Exp Point
maxPoint (T2 x1 y1) (T2 x2 y2) =
  cond (x1 < x2 ||! (x1 == x2 &&! y1 < y2)) (T2 x2 y2) (T2 x1 y1)
