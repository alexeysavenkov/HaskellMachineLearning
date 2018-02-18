{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, FlexibleInstances #-}

import LevmarOptimizer
import ChartUtils
import EnergyCostDataset

-- Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Types

optimizeAndChart f trainingData initialParams =
  let 
    (optimizedF, _, _) = optimizeModel f trainingData initialParams
  in do
    layout_y_axis . laxis_generate .= scaledAxis def (-10, 80)
    layout_x_axis . laxis_generate .= scaledAxis def (-10, 80)
    layout_title .= "Linear Regression"
    --plotFunction optimizedF "kek" --(line "am" [signal [0,(0.5)..400]])
    

    setColors (map opaque [red, green])
    setShapes [PointShapePolygon 4 True]
    
    plot (customPoints "am points" trainingData 10)

--models :: HyperParams hp => [(hp -> Double -> Double, String)]
linear (a, b) x = a*x + b
quadratic (a, b, c) x = a*x*x + b*x + c
cubic (a,b,c,d) x = a*x*x*x + b*x*x + c*x + d
asymptotic (a,b,c) x = a + b* (exp (c*x))
logisticPopulation (a,b,c) x = a/(1 + exp(b + c*x))


plotModels =
  let 
    (linearModel, _, _) = optimizeModel linear energyCostTrainingSet [0,0]
    (quadraticModel, _, _) = optimizeModel quadratic energyCostTrainingSet [0,0,0]
    (cubicModel, _, _) = optimizeModel cubic energyCostTrainingSet [0,0,0,0]
    (asympModel, _, _) = optimizeModel asymptotic energyCostTrainingSet [0,0,0]
    (logisticModel, _, _) = optimizeModel logisticPopulation energyCostTrainingSet [0,0,0]

    formattedPlot model label =
      let
        trainingLoss = round $ loss model energyCostTrainingSet
        totalLoss = round $ loss model energyDataset
        formattedLabel = label ++ " (" ++ (show trainingLoss) ++ "|" ++ (show totalLoss) ++ ")"
      in plotFunction model formattedLabel [0,(0.005)..5]
      
   in do
    layout_y_axis . laxis_generate .= scaledAxis def (0, 150)
    layout_x_axis . laxis_generate .= scaledAxis def (0, 5)
    setColors (map opaque [red, green, blue, yellow, cyan, violet, purple])
    setShapes [PointShapeCircle, PointShapeCircle, PointShapeCircle]
    formattedPlot linearModel "Linear"
    formattedPlot quadraticModel "Quadratic"   
    formattedPlot asympModel "Asymptotic"
    formattedPlot logisticModel "Logistic"
    formattedPlot cubicModel "Cubic"
    plot (customPoints "Training set" energyCostTrainingSet 5) 
    plot (customPoints "Test set" energyCostTestSet 5)
    






