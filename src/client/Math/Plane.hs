module Math.Plane where

import Linear
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe (isJust, fromJust)
import Data.List (nubBy)

vec3 :: a -> V3 a
vec3 a = V3 a a a

class ApproxEq a where
  approxEq :: a -> a -> Bool

instance ApproxEq (V3 Float) where
  approxEq v1 v2 = norm (v2 - v1) < 0.000001

instance ApproxEq Float where
  approxEq v1 v2 = abs (v2 - v1) < 0.000001

nubVecs :: Vector (V3 Float) -> Vector (V3 Float)
nubVecs = V.fromList . nubBy approxEq . V.toList

signedAngle :: V3 Float -> V3 Float -> Float
signedAngle a b = s * acos (a' `dot` b')
  where
    a' = normalize a
    b' = normalize b
    s  = signum $ triple a b (a `cross` b)

catMaybes :: Vector (Maybe a) -> Vector a
catMaybes = fmap fromJust . V.filter isJust

data Plane = Plane {
  planeOrigin :: !(V3 Float)
, planeNormal :: !(V3 Float)
, planeTangent :: !(V3 Float)
}

-- | Bitangent vector of plane (second coplanar vector of plane)
planeBitangent :: Plane -> V3 Float
planeBitangent Plane{..} = normalize $ planeTangent `cross` planeNormal

-- | Construct plane from thre points
planeFromPoints :: V3 Float -> V3 Float -> V3 Float -> Plane
planeFromPoints v1 v2 v3 = Plane v1 n t
  where
  t = normalize $ v2 - v1
  n = normalize $ t `cross` (v3 - v1)

-- | Convert plane point into 3D points
planePoint :: Plane -> V2 Float -> V3 Float
planePoint p (V2 x y) = planeOrigin p + fmap (x *) (planeTangent p) + fmap (y *) (planeBitangent p)

-- | Calculate distance from point to nearest point of plane
distanceToPlane :: Plane -> V3 Float -> Float
distanceToPlane Plane{..} v = norm $ project planeNormal (v - planeOrigin)

-- | Test if point belongs to the plane
pointInPlane :: V3 Float -> Plane -> Bool
pointInPlane v p = distanceToPlane p v < 0.0001

data CubeSide = CubeLeft | CubeRight | CubeFront | CubeBack | CubeBottom | CubeTop
  deriving (Eq, Show)

-- | All planes of axis aligned cube
cubePlanes :: V3 Float -> Float -> Vector (Plane, CubeSide)
cubePlanes o d = V.fromList [
    (Plane o (V3 1 0 0) (V3 0 0 1), CubeLeft)
  , (Plane o (V3 0 0 (-1)) (V3 1 0 0), CubeFront)
  , (Plane o (V3 0 1 0) (V3 1 0 0), CubeBottom)
  , (Plane (o + vec3 d) (V3 (-1) 0 0) (V3 0 (-1) 0), CubeRight)
  , (Plane (o + vec3 d) (V3 0 0 (-1)) (V3 (-1) 0 0), CubeBack)
  , (Plane (o + vec3 d) (V3 0 (-1) 0) (V3 0 0 (-1)), CubeTop)
  ]

-- | Transform cube side to same side but within external cube
swapSide :: CubeSide -> CubeSide
swapSide cs = case cs of
  CubeLeft -> CubeRight
  CubeRight -> CubeLeft
  CubeFront -> CubeBack
  CubeBack -> CubeFront
  CubeBottom -> CubeTop
  CubeTop -> CubeBottom

data Line = Line {
  lineOrigin :: !(V3 Float)
, lineTangent :: !(V3 Float)
} deriving (Show)

linePoint :: Line -> Float -> V3 Float
linePoint Line{..} a = lineOrigin + fmap (a *) lineTangent

-- | Check if given line is completly belongs to plane
lineInPlane :: Line -> Plane -> Bool
lineInPlane Line{..} p@Plane{..} = (lineOrigin `pointInPlane` p) && cosAngle < 0.001
  where
    cosAngle = abs $ planeNormal `dot` lineTangent

-- | Return all edges of cube with given origin and size
gridEdges :: V3 Float -> Float -> Vector Line
gridEdges o d = V.fromList [
    Line o (V3 1 0 0)
  , Line o (V3 0 1 0)
  , Line o (V3 0 0 1)
  , Line (o + vec3 d) (V3 (-1) 0 0)
  , Line (o + vec3 d) (V3 0 (-1) 0)
  , Line (o + vec3 d) (V3 0 0 (-1))
  , Line (o + V3 0 d 0) (V3 1 0 0)
  , Line (o + V3 0 d 0) (V3 0 0 1)
  , Line (o + V3 d 0 d) (V3 (-1) 0 0)
  , Line (o + V3 d 0 d) (V3 0 0 (-1))
  , Line (o + V3 d 0 0) (V3 0 1 0)
  , Line (o + V3 0 0 d) (V3 0 1 0)
  ]

-- | Return cube cut for cube with given origin and size
gridCut :: Plane -> V3 Float -> Float -> Vector (V3 Float)
gridCut plane o d = catMaybes $ intersect <$> gridEdges o d
  where
    intersect l = if l `lineInPlane` plane then Nothing else let
      a = lineCrossPlane l plane
      in if (a > 0 || a `approxEq` 0) && (a < d || a `approxEq` d) then Just (linePoint l a) else Nothing

-- | Project 3D point into plane
planeProject :: Plane -> V3 Float -> V2 Float
planeProject p@Plane{..} v = V2 b c
  where
  dv = v - planeOrigin -- 3d vector from origin to point
  dvn = project planeNormal dv -- Projection to normal
  dvt = dv - dvn -- Tangent projection
  bv = project planeTangent dvt -- Project on tangent plane vector
  b = signum (bv `dot` planeTangent) * norm bv
  cv = project (planeBitangent p) dvt -- Project on bitangent plane vector
  c = signum (cv `dot` planeBitangent p) * norm cv

isInTriangle2D :: V2 Float -- ^ First point
  -> V2 Float -- ^ Second point
  -> V2 Float -- ^ Third point
  -> V2 Float -- ^ Test point
  -> Bool
isInTriangle2D v1 v2 v3 vt = (b1 == b2) && (b2 == b3)
  where
  signp (V2 p1x p1y) (V2 p2x p2y) (V2 p3x p3y) = (p1x - p3x) * (p2y - p3y) - (p2x - p3x) * (p1y - p3y)
  b1 = let val = signp vt v1 v2 in val < 0 || val `approxEq` 0
  b2 = let val = signp vt v2 v3 in val < 0 || val `approxEq` 0
  b3 = let val = signp vt v3 v1 in val < 0 || val `approxEq` 0

isInTriangle :: V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> V3 Float -- ^ Test point
  -> Bool
isInTriangle a b c v = isInTriangle2D a' b' c' v'
  where
  plane = planeFromPoints a b c
  a' = V2 0 0
  b' = planeProject plane b
  c' = planeProject plane c
  v' = planeProject plane v

-- | Points that triangle cut cube (doesn't include borders of triangle)
triangleCut :: V3 Float -- ^ First point
  -> V3 Float -- ^ Second point
  -> V3 Float -- ^ Third point
  -> V3 Float -- ^ Cube origin
  -> Float -- ^ Cube size
  -> Vector (V3 Float)
triangleCut a b c o d = V.filter (isInTriangle a b c) $ gridCut plane o d
  where
  plane = planeFromPoints a b c

-- | Construct line from two points
lineFromPoints :: V3 Float -> V3 Float -> Line
lineFromPoints lbegin lend = Line lbegin (normalize $ lend - lbegin)

-- | Calclulate line and plane parameters for crossing point
lineCrossPlane :: Line -> Plane -> Float
lineCrossPlane Line{..} p@Plane{..} = a
  where
  V3 vlx vly vlz = lineOrigin
  V3 tlx tly tlz = lineTangent
  V3 vp0x vp0y vp0z = planeOrigin
  V3 tpx tpy tpz = planeTangent
  V3 btpx btpy btpz = planeBitangent p
  a = (((vp0z-vlz)*tpy-tpz*(vp0y-vly))*btpx+((-vp0z+vlz)*tpx+tpz*(vp0x-vlx))*btpy-((-vp0y+vly)*tpx+tpy*(vp0x-vlx))*btpz)/((-tly*tpz+tlz*tpy)*btpx+(tlx*tpz-tlz*tpx)*btpy-btpz*(tlx*tpy-tly*tpx))

-- | Cross line and plane with given restriction for tangent lengths
lineCrossPlaneRestrict :: Line -> Float -> Plane -> Float -> Maybe (V3 Float)
lineCrossPlaneRestrict l lineLength p planeLength = let
  a = lineCrossPlane l p
  V2 b c = planeProject p (linePoint l a)
  in if a > 0 && a <= lineLength && b >= 0 && b <= planeLength && c >= 0 && c <= planeLength
        then Just $ lineOrigin l + fmap (a *) (lineTangent l)
        else Nothing

-- | Detect cross line and axis aligned box
lineCrossBoxRestrict :: Line -> Float -> V3 Float -> Float -> Maybe (V3 Float, CubeSide)
lineCrossBoxRestrict l lineLength boxOrigin boxSize = let
  tries = try <$> cubePlanes boxOrigin boxSize
  try (p, s) = if l `lineInPlane` p then Nothing
    else case lineCrossPlaneRestrict l lineLength p boxSize of
      Nothing -> Nothing
      Just v -> Just (v, s)
  succs = catMaybes tries
  in succs V.!? 0

-- | Returns True if all points lies on single line
colinear :: V3 Float -> V3 Float -> V3 Float -> Bool
colinear a b c = let s = norm ((b - a) `cross` (c - a)) in s < 0.000001

-- | Test if all vectors in the vector are not forming triangles
colinearAll :: Vector (V3 Float) -> Bool
colinearAll vs
  | V.length vs < 3 = False
  | otherwise = muts
  where
    muts = and $ V.concatMap (\a -> V.concatMap (\b -> fmap (colinear a b) . V.filter (\v -> (v /= a) && (v /= b)) $ vs ) . V.filter (/= a) $ vs) vs

-- | Test if all vectors in the vector are not forming triangles
colinearAll2D :: Vector (V2 Float) -> Bool
colinearAll2D vs2
  | V.length vs2 < 3 = False
  | otherwise = muts
  where
    vs = fmap (\(V2 x y) -> (V3 x y 0)) vs2
    muts = and $ V.concatMap (\a -> V.concatMap (\b -> fmap (colinear a b) . V.filter (\v -> (v /= a) && (v /= b)) $ vs ) . V.filter (/= a) $ vs) vs
