
module Main where

import StationXML
import Text.XML.Light

main :: IO ()
main = do
  t <- fmap parseXMLDoc $ readFile "example.xml"
  case t of
    Nothing    -> error "Could not parse example StationXML file"
    Just elems -> do
      let showChn = show
      let showStn s = show s ++ (unwords . map showChn $ channels s)
      let showNet n = show n ++ (unwords . map showStn $ stations n)
      let nets = parseStationXML elems
      mapM_ (putStrLn . showNet) nets
