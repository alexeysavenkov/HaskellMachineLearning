module LAB01SplineApproximation where
  
import Data.List
import Data.Maybe
import ChartUtils
import EnergyCostDataset
import Loss
import Debug.Trace

-- Chart
import Graphics.Rendering.Chart.Easy hiding (Point)
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Types

import Diagrams.CubicSpline
import Data.Algorithm.CubicSpline

import Data.Spline.Curve
import Bezier hiding (Point)


type Point = (Double, Double)

mult :: Double -> Point -> Point
mult k (x, y) = (k*x, k*y)

plus :: Point -> Point -> Point
plus (a,b) (c,d) = (a+c,b+d)

average xs = realToFrac (sum xs) / genericLength xs

searchAcc :: Double -> [Point] -> [Point] -> ([Point], Maybe Point, [Point]) 
searchAcc queriedX [] zipperLeft = (zipperLeft, Nothing, [])
searchAcc queriedX (x:xs) zipperLeft
  | queriedX == fst x   = (zipperLeft, Just x, xs)
  | queriedX > fst x    = searchAcc queriedX xs (x:zipperLeft)
  | queriedX < fst x    = (zipperLeft, Nothing, (x:xs))

zipperSearch :: Double -> [Point] -> ([Point], Maybe Point, [Point]) 
zipperSearch x pts = searchAcc x pts []

trainingDataZipperSearch x trainingData =
  let 
    yByX = map (\xs -> (fst . head $ xs, average (map snd xs))) . groupBy (\x y -> (fst x) == (fst y)) . sort $ trainingData
  in zipperSearch x yByX

trainLinearSpline :: [Point] -> (Double -> Double)
trainLinearSpline trainingData =
    \x -> 
      let 
        zipperResult = trainingDataZipperSearch x trainingData
      in 
        case zipperResult of 
          (_, Just (_, y), _) -> y
          (((lx,ly):_), _, ((rx,ry):_)) -> ly + (x - lx) * ((ry - ly)/(rx - lx))
          ([], _, ((lx,ly):(rx,ry):_)) -> ly + (x - lx) * ((ry - ly)/(rx - lx))
          ((lx,ly):(rx,ry):_, _, []) -> ly + (x - lx) * ((ry - ly)/(rx - lx))
          

debug :: Show a => a -> a
debug x = (trace (show x) x)



cubicSplineCoeffs = cubicSplineCoefficients energyCostTrainingSet

getCubicBezierPoints :: (Point, Point, Point, Point) -> [Double] -> [Point]
getCubicBezierPoints (p0,p1,p2,p3) [u0,u1,u2,u3] =
  let 
    b t = 
      ((u0*((1-t)**3)) `mult` p0) `plus`
      ((u1*3*((1-t)**2)*t) `mult` p1) `plus` 
      ((u2*3*(1-t)*(t**2)) `mult` p2) `plus` 
      ((u3*(t**3)) `mult` p3)
  in map b [0,(0.0001)..1]

cubicBezierWindows :: [a] -> [[a]]
cubicBezierWindows (a:b:c:d:xs) = ([a,b,c,d]:(cubicBezierWindows (b:c:d:xs)))
cubicBezierWindows _ = []

cubicBezier :: [Point]
cubicBezier = 
    ( (zip (cubicBezierWindows (sort energyCostTrainingSet)) cubicSplineCoeffs)) 
      >>= (\([a@(initX,initY),b@(lastX,_),c,d], ks) -> 
              let 
                rawPts@((_,invalidY):_) = getCubicBezierPoints (a,b,c,d) ks
                diffY = initY - invalidY
              in map (\(x,y) -> (x, y+diffY)) $ filter (\(x,y) -> x > initX && x < lastX) rawPts
    )


trainCubicSpline :: [Point] -> (Double -> Double)
trainCubicSpline trainingData =
    \x -> 
      let 
        zipperResult = trainingDataZipperSearch x trainingData
      in 
        case zipperResult of 
          (_, Just (_, y), _) -> y
          (((lx,ly):_), _, ((rx,ry):_)) -> 
            case filter (\(a,b) -> a >= x) cubicBezier of
               ((a,b):_) -> b
               [] -> 0
          
          ([], _, ((lx,ly):(rx,ry):_)) -> ly + (x - lx) * ((ry - ly)/(rx - lx))
          ((lx,ly):(rx,ry):_, _, []) -> ly + (x - lx) * ((ry - ly)/(rx - lx))
          


every n xs = case drop (n-1) xs of
              (y:ys) -> y : every n ys
              [] -> []
    
linearSpline = trainLinearSpline energyCostTrainingSet
cubSpline = trainCubicSpline energyCostTrainingSet
bezierSpline = bezier2d (every 2 energyCostTrainingSet)


linearSplinePlot = do 
  layout_title .= "Regressions"
  layout_y_axis . laxis_generate .= scaledAxis def (0, 150)
  layout_x_axis . laxis_generate .= scaledAxis def (0, 5)
  setColors (map opaque [red, green, {-blue, yellow, cyan,-} violet, purple])
  setShapes [PointShapeCircle, PointShapeCircle, PointShapeCircle]
  formattedPlot linearSpline "Linear Spline" [0,(0.005)..5]
  --formattedPlot bezierSpline "Bezier Spline" [0,(0.005)..5]
  --formattedPlot cubSpline "Cubic Spline" [0,(0.005)..5]
  plot (customPoints "Training set" energyCostTrainingSet 5) 
  plot (customPoints "Test set" energyCostTestSet 5)
  

      