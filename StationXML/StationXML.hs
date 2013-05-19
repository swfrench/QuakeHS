
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
  { channelName  :: String
  , channelStart :: String
  , channelEnd   :: String
  , channelLoc   :: String
  , channelLat   :: Float
  , channelLon   :: Float
  , channelDep   :: Float
  , channelDip   :: Float
  , channelAzm   :: Float
  } deriving (Show, Eq)

-- | A single station, associated StationXML attribute metadata and 'Channel' children
data Station = Station
  { stationName  :: String
  , stationStart :: String
  , stationEnd   :: String
  , channels     :: [Channel]
  } deriving (Show, Eq)

-- | A single network, associated StationXML attribute metadata and 'Station' children
data Network = Network
  { networkName  :: String
  , networkStart :: String
  , networkEnd   :: String
  , stations     :: [Station]
  } deriving (Show, Eq)

getAttr :: String -> Element -> String
getAttr s = fromMaybe "" . findAttrBy (\q -> qName q == s)

getCode :: Element -> String
getCode = getAttr "code"

getLocation :: Element -> String
getLocation = getAttr "locationCode"

getStart :: Element -> String
getStart = getAttr "startDate"

getEnd :: Element -> String
getEnd = getAttr "endDate"

getChildren :: String -> Element -> [Element]
getChildren s = filterChildrenName (\e -> qName e == s)

getNetworks :: Element -> [Element]
getNetworks = getChildren "Network"

getStations :: Element -> [Element]
getStations = getChildren "Station"

getChannels :: Element -> [Element]
getChannels = getChildren "Channel"

parseNetwork :: (Element -> [Station]) -> Element -> Network
parseNetwork stns n = Network (getCode n) (getStart n) (getEnd n) (stns n)

parseStation :: (Element -> [Channel]) -> Element -> Station
parseStation chns s = Station (getCode s) (getStart s) (getEnd s) (chns s)

getChildContent :: String -> Element -> String
getChildContent n = maybe "" strContent . filterChildName (\c -> qName c == n)

parseChannel :: Element -> Channel
parseChannel c =
  let lat = read . getChildContent "Latitude" $ c
      lon = read . getChildContent "Longitude" $ c
      dep = read . getChildContent "Depth" $ c
      dip = read . getChildContent "Dip" $ c
      azm = read . getChildContent "Azimuth" $ c
  in  Channel (getCode c) (getStart c) (getEnd c) (getLocation c) lat lon dep dip azm

-- | Return a list of 'Network' structures present in the 'Element' tree returned by @parseXMLDoc@ from the @Text.XML@ module
parseStationXML
  :: Element    -- ^ Root of the StationXML (sub-)tree containing Network elements
  -> [Network]  -- ^ Returns: List of 'Network' structures
parseStationXML elems =
  let nets = map (parseNetwork stns) . getNetworks
      stns = map (parseStation chns) . getStations
      chns = map  parseChannel . getChannels
  in  nets elems
