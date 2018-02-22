module Loss where
  
type Loss = Double

loss :: (Double -> Double) -> [(Double, Double)] -> Loss
loss f testData = 
  let
    xs = map fst testData
    expectedResults = map snd testData
    actualResults = map f xs
    sqrDiffs = map (\x -> x*x) $ zipWith (-) expectedResults actualResults
  in sum sqrDiffs