module EnergyCostDataset where
  
import Data.List

-- (Labour price, Energy price)
energyDataset :: [(Double, Double)]
energyDataset = sort [
    (0.386, 13.219),
    (1.179, 49.145),
    (0.532, 18.005),
    (1.836, 75.639),
    (1.136, 52.234),
    (1.085, 9.027),
    (2.390, 41.676),
    (1.356, 31.244),
    (0.115, 1.739),
    (2.591, 104.584),
    (2.789, 82.296),
    (0.933, 21.990),
    (4.004, 125.351),
    (1.513, 43.232),
    (2.540, 75.581),
    (1.416, 42.037)
  ]
  
takeOddElems xs = map fst $ filter (odd . snd) $ zip xs [1..]
takeEvenElems xs = map fst $ filter (even . snd) $ zip xs [1..]
  
energyCostTrainingSet = takeOddElems energyDataset
energyCostTestSet = takeEvenElems energyDataset
