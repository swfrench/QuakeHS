
module Main where

import WebService

testStationQuery :: Query
testStationQuery = 
  [ ("starttime", "2013-01-01")
  , ("endtime"  , "2013-01-02")
  , ("network"  , "IU")
  , ("station"  , "ANMO")
  , ("level"    , "channel")
  , ("nodata"   , "404")]

main :: IO ()
main = do 
  putStrLn $ "supported services: " ++ show supportedServices
  putStrLn "running test query ..."
  resp <- runQuery "station" testStationQuery 
  case resp of
    Nothing -> error "test query failed!"
    Just r  -> putStrLn "success" >> print r
