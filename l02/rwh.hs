module Points where

-- Exercise 9
--
data Direction = Left | Straight | Right
    deriving (Show)


-- Exercise 10
--
data Point = Point Double Double
    deriving (Show)


angle :: Point -> Point -> Point -> Double
angle (Point aX aY) (Point bX bY) (Point cX cY) =
    angleA - angleB
    where angleA = atan2 (aY - bY) (aX - bX)
          angleB = atan2 (cY - bY) (cX - bX)


toDegrees :: (Num a, Floating a) => a -> a
toDegrees r = r * 180 / pi


direction :: Point -> Point -> Point -> Direction
direction pointA pointB pointC
    | ang > 180  = Points.Left
    | ang == 180 = Points.Straight
    | ang < 180  = Points.Right
    where ang = abs $ toDegrees $ angle pointA pointB pointC


-- Exercise 11
--
listDirection :: [Point] -> [Direction]
listDirection (x:y:z:zs) = direction x y z : listDirection (y:z:zs)
listDirection _          = []
