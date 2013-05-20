
module Main where

import StationXML
import Text.XML.Light

main :: IO ()
main = do
  t <- fmap parseXMLDoc $ readFile "example.xml"
  case t of
    Nothing    -> error "Could not parse example StationXML file"
    Just elems ->
      let showNet n = show n ++ (unlines . map show . stations $ n)
      in  putStrLn . unlines . map showNet . parseStationXML $ elems
