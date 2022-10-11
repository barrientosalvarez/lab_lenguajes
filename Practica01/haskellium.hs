module Haskellium where 

import Shape 
import Point

data Haskellium=Haskellium
                {name       :: String
                ,lastName1  :: String
                ,lastName2  :: String
                ,location   :: Point
                ,houseShape :: Shape}



-- Metodos auxiliares

-- Getters
getLastName1 :: Haskellium->String
getLastName1 h = lastName1 h

getLastName2 :: Haskellium->String
getLastName2 h = lastName2 h

getLocation :: Haskellium->Point
getLocation h = location h

getHouseShape :: Haskellium->Shape
getHouseShape h = houseShape h
