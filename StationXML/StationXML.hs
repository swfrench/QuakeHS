
-- | Limited support for FDSN StationXML parsing, including:
--
-- * station, network, channel start and end times
--
-- * channel location codes
--
-- * station physical location (lat,lon,depth) and orientation (dip,azimuth)
--
-- Clearly a small subset of the full StationXML spec
-- (<http://www.fdsn.org/xml/station/>) but easily extensible as more
-- functionality becomes necessary, e.g. pole-zero responses.
module StationXML
( Network (..)
, Station (..)
, Channel (..)
, parseStationXML
) where

import Data.Maybe (fromMaybe)
import Text.XML.Light

-- | A single channel and a subset of associated StationXML (attribute or child) metadata
data Channel = Channel
  { channelName   :: String
  , channelStart  :: String
  , channelEnd    :: String
  , channelStatus :: String
  , channelLoc    :: String
  , channelLat    :: Float
  , channelLon    :: Float
  , channelDep    :: Float
  , channelDip    :: Float
  , channelAzm    :: Float
  } deriving (Show, Eq)

-- | A single station, associated StationXML attribute metadata and 'Channel' children
data Station = Station
  { stationName   :: String
  , stationStart  :: String
  , stationEnd    :: String
  , stationStatus :: String
  , channels      :: [Channel]
  } deriving (Eq)

-- Custom show instance for Station
instance Show Station where
  show n = unlines . map unwords $
    [ ["* Station",  stationName   n]
    , ["    start:", stationStart  n]
    , ["      end:", stationEnd    n]
    , ["   status:", stationStatus n]
    , ["   contains", show . length . channels $ n, "channels"]]


-- | A single network, associated StationXML attribute metadata and 'Station' children
data Network = Network
  { networkName   :: String
  , networkStart  :: String
  , networkEnd    :: String
  , networkStatus :: String
  , stations      :: [Station]
  } deriving (Eq)

-- Custom show instance for Network
instance Show Network where
  show n = unlines . map unwords $
    [ ["* Network",  networkName   n]
    , ["    start:", networkStart  n]
    , ["      end:", networkEnd    n]
    , ["   status:", networkStatus n]
    , ["   contains", show . length . stations $ n, "stations"]]


getAttr :: String -> Element -> String
getAttr s = fromMaybe "" . findAttrBy ((==) s . qName)

getChildren :: String -> Element -> [Element]
getChildren s = filterChildrenName ((==) s . qName)

getChildContent :: String -> Element -> String
getChildContent s = maybe "" strContent . filterChildName ((==) s . qName)

parseNetwork :: (Element -> [Station]) -> Element -> Network
parseNetwork stns n =
  let code  = getAttr "code"             n
      start = getAttr "startDate"        n
      end   = getAttr "endDate"          n
      stat  = getAttr "restrictedStatus" n
  in  Network code start end stat (stns n)

parseStation :: (Element -> [Channel]) -> Element -> Station
parseStation chns s =
  let code  = getAttr "code"             s
      start = getAttr "startDate"        s
      end   = getAttr "endDate"          s
      stat  = getAttr "restrictedStatus" s
  in  Station code start end stat (chns s)

parseChannel :: Element -> Channel
parseChannel c =
  let code  = getAttr "code"             c
      start = getAttr "startDate"        c
      end   = getAttr "endDate"          c
      loc   = getAttr "locationCode"     c
      stat  = getAttr "restrictedStatus" c
      lat   = readContent "Latitude"     c
      lon   = readContent "Longitude"    c
      dep   = readContent "Depth"        c
      dip   = readContent "Dip"          c
      azm   = readContent "Azimuth"      c
      readContent s = read . getChildContent s
  in  Channel code start end stat loc lat lon dep dip azm

-- | Return a list of 'Network' structures present in the 'Element' tree returned by @parseXMLDoc@ from the @Text.XML@ module
parseStationXML
  :: Element    -- ^ Root of the StationXML (sub-)tree containing Network elements
  -> [Network]  -- ^ Returns: List of 'Network' structures
parseStationXML elems =
  let nets = map (parseNetwork stns) . getChildren "Network"
      stns = map (parseStation chns) . getChildren "Station"
      chns = map  parseChannel       . getChildren "Channel"
  in  nets elems
