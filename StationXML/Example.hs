
module Main where

import StationXML
import Text.XML.Light

main :: IO ()
main = do
  t <- fmap parseXMLDoc $ readFile "example.xml"
  putStrLn $ maybe "" (unlines . map show . parseStationXML) t
