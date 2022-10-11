module Point where

type X=Float
type Y=Float

data Point= Point X Y

distance::Point->Point->Float
distance (Point a1 b1) (Point a2 b2)=sqrt((a1-a2)^2+(b1-b2)^2)

from0::Point->Float
from0 (Point x y)=sqrt((0-x)^2+(0-y)^2)
