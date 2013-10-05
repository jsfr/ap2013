module Curves where

-- Imports
import Text.Printf (printf)

-- Types
data Point = Point Double Double deriving Show
type Curve = [Point]
data Axis = Vertical | Horizontal

-- Typeclasses
instance Eq Point where
  Point x1 y1 == Point x2 y2 = abs (x1 - x2) < 0.01 && abs (y1 - y2) < 0.01

-- Constructor functions
point :: (Double, Double) -> Point
point (x,y) = Point x y

curve :: Point -> [Point] -> Curve
curve p ps = p : ps

-- Curve functions
connect :: Curve -> Curve -> Curve
connect c1 c2
  | last c1 == head c2 = c1 ++ tail c2
  | otherwise = c1 ++ c2

rotate :: Curve -> Double -> Curve
rotate c d = map  (\(Point x y) -> Point (y * sinr - x * cosr) ((-x) * sinr - y * cos r)) c
  where r = d * pi / 180
        sinr = sin r
        cosr = cos r

translate :: Curve  -> Point -> Curve
translate [] _ = []
translate css@(Point cx cy:_) (Point px py) = map (\(Point x y) -> Point (x + dx) (y + dy)) css
  where dx = px - cx
        dy = py - cy

reflect :: Curve -> Axis -> Double -> Curve
reflect c Vertical a = map (\(Point x y) -> Point (-x+2*a) y) c
reflect c Horizontal a = map (\(Point x y) -> Point x (-y+2*a)) c

bbox :: Curve -> (Point, Point)
bbox [] = (Point 0 0, Point 0 0)
bbox (c:cs) = ( foldl pmin c cs, foldl pmax c cs )
  where pmax (Point x1 y1) (Point x2 y2) = Point (max x1 x2) (max y1 y2)
        pmin (Point x1 y1) (Point x2 y2) = Point (min x1 x2) (min y1 y2)

width :: Curve -> Double
width c = (\(Point x1 _, Point x2 _) -> abs $ x1-x2) (bbox c) 

height :: Curve -> Double
height c = (\(Point _ y1, Point _ y2) -> abs $ y1-y2) (bbox c)

-- Converter functions
toList :: Curve -> [Point]
toList c = c

toSvg :: Curve -> String
toSvg c = start ++ concatMap line (zip c' $ tail c') ++ end
  where c' = move (fst $ bbox c) c
        pp = printf "%.2f"
        size = "width=\"" ++ show (ceiling $ width c  :: Int) ++
               "px\" height=\"" ++ show (ceiling $ height c :: Int) ++ "px\""
        start = "<svg xmlns=\"http://www.w3.org/2000/svg\" " ++ size ++
                " version=\"1.1\"><g>"
        end = "</g></svg>"
        move (Point dx dy) = map (\(Point x y) -> Point (x - dx) (y - dy))
        line (Point x1 y1, Point x2 y2) =
          "<line style=\"stroke-width: 2px; stroke: black; fill:white\" " 
          ++ "x1=\"" ++ pp x1 ++ "\" x2=\"" ++ pp x2 ++ "\" "
          ++ "y1=\"" ++ pp y1 ++ "\" y2=\"" ++ pp y2 ++ "\" />"

toFile :: Curve -> FilePath -> IO ()
toFile c p = writeFile p $ toSvg c