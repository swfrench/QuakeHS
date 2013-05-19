
module Main where

import Sphere

main :: IO ()
main = do 
    print $ shoot (Path (Point 0 0)   40  160) 2
    print $ shoot (Path (Point 0 0) (-40) 160) 2 
