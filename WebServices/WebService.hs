
-- | A simple interface for the IRIS DMC Web Services <http://service.iris.edu/>
--
-- Currently, only the @station@ service is supported (@dataselect@ will be added)
module WebService 
( Query
, supportedServices
, runQuery
) where

import qualified Data.Map as M
import Network.URI
import Network.HTTP
import qualified Data.ByteString as B

-- | Convenient type alias for the query key-value association list
type Query = [(String,String)]

ok :: ResponseCode
ok = (2,0,0)

supportedServiceUrls :: M.Map String String
supportedServiceUrls = M.fromList 
  [("station", "http://service.iris.edu/fdsnws/station/1")]

-- | Services currently supported
supportedServices :: [String]
supportedServices = M.keys supportedServiceUrls

generateRequest 
  :: String 
  -> Query 
  -> Maybe URI
generateRequest s q = 
  case M.lookup s supportedServiceUrls of
    Nothing -> error $ "Unsupported service " ++ s
    Just u  -> parseURI $ u ++ "/query?" ++ urlEncodeVars q

-- | Run the specified 'Query' for the desired service
runQuery 
  :: String                    -- ^ Service name
  -> Query                     -- ^ Query parameters (key-value pairs)
  -> IO (Maybe B.ByteString)   -- ^ Returns: @Maybe@ server response (string @ByteString@) in the @IO@ Monad
runQuery s q =
  case generateRequest s q of
    Nothing  -> 
      error "Could not parse resulting URI - check your query"
    Just u -> 
      do resp <- simpleHTTP $ mkRequest GET u
         code <- getResponseCode resp
         if code == ok then fmap Just $ getResponseBody resp
                       else return Nothing
