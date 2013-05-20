
-- | Tools for geometric calculations on the sphere often required in seismology
--
-- All angles (latitudes, longitudes, distances, etc...) are in degrees
module Sphere
( Point (..)
, Path (..)
, antipode
, toGeographic
, toGeocentric
, path
, flipPath
, endPoint
, shoot
) where

-- | A point on the sphere, characterized by longitude and latitude
data Point = Point
  { longitude :: Double
  , latitude  :: Double
  } deriving (Show, Eq, Ord)

-- | A great-circle path on the sphere, characterized by an anchor point, distance, and azimuth
data Path = Path
  { anchor   :: Point
  , azimuth  :: Double
  , distance :: Double
  } deriving (Show, Eq)

argClean :: Double -> Double
argClean = min 1.0 . max (-1.0)

bound :: Double -> Double -> Double -> Double
bound x x0 x1
    | x <  x0   = bound (x + dx) x0 x1
    | x >= x1   = bound (x - dx) x0 x1
    | otherwise = x
    where dx = x1 - x0

radians :: Double -> Double
radians = (*) (pi / 180.0)

degrees :: Double -> Double
degrees = (*) (180.0 / pi)

sind:: Double -> Double
sind = sin . radians

cosd :: Double -> Double
cosd = cos . radians

acosd :: Double -> Double
acosd = degrees . acos . argClean

atan2d :: (Double, Double) -> Double
atan2d = degrees . uncurry atan2

flatFactor :: Double
flatFactor = 0.993305621334896

geocentric :: Double -> Double
geocentric x = atan2d (flatFactor * cosd (90.0 - x), sind (90.0 - x))

geographic :: Double -> Double
geographic x = 90.0 - atan2d (cosd x, flatFactor * sind x)

-- | Returns the antipode to a supplied point
antipode :: Point -> Point
antipode (Point lon lat) =
  Point { longitude = bound (lon + 180) 0 360, latitude = - lat }

-- | Maps from geographic to geocentric coordinates
toGeocentric :: Point -> Point
toGeocentric (Point lon lat) =
  Point { longitude = lon, latitude = geocentric lat }

-- | Maps from geocentric to geographic coordinates
toGeographic :: Point -> Point
toGeographic (Point lon lat) =
  Point { longitude = lon, latitude = geographic lat }

-- | Calculate great-circle path between two points (azimuth measured CW from N)
path :: Point -> Point -> Path
path p@(Point lon0 lat0) (Point lon1 lat1) =
    let clat0 = 90.0 - lat0
        clat1 = 90.0 - lat1
        dlon  = lon1 - lon0
        delta = acosd $  cosd clat0 * cosd clat1 + sind clat0  *  sind clat1 * cosd dlon
        az    = acosd $ (cosd clat1 - cosd clat0 * cosd delta) / (sind clat0 * sind delta)
    in  Path
        { anchor   = p
        , distance = delta
        , azimuth  = if sind dlon > 0 then az else (- az) }

-- | Given a path, flip from minor to major arc, or vice versa
flipPath :: Path -> Path
flipPath (Path p az delta) =
  Path p (bound (az + 180) (-180) 180) (360 - delta)

-- | Given a path, infer the end point thereof.
--
-- Note: 'Path' distance must be strictly positive.
endPoint :: Path -> Point
endPoint (Path (Point lon0 lat0) az delta)
    | delta <= 0 = error "Path distance <= 0 not supported"
    | otherwise  =
        let clat0 = 90.0 - lat0
            clat1 = acosd $ cosd clat0 * cosd delta + sind clat0 * sind delta * cosd az
            dlon  = acosd $ (cosd delta - cosd clat0 * cosd clat1) / (sind clat0 * sind clat1)
            lon1  = lon0 + (signum az * signum (180 - delta)) * dlon
        in  Point
            { longitude  = bound lon1 0 360
            , latitude   = 90.0 - clat1 }

-- | Given a path, enumerate n equally-spaced (in terms of great-circle distance) waypoints
shoot :: Path -> Int -> [Point]
shoot (Path p az delta) n
    | n < 2     = error "Less than 2 waypoints requested"
    | otherwise =
        let dx     = delta / fromIntegral (n - 1)
            deltas = map (* dx) [1 .. fromIntegral n-1]
            paths  = map (Path p az) deltas
        in  p : map endPoint paths
