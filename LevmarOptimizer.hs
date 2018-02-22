{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}

module LevmarOptimizer where
  
import qualified Data.Vector.Storable as V
import Data.Vector.Storable ((!))
import           Numeric.LevMar

import ChartUtils
import Loss

class HyperParams p where
  extractFromVec :: (V.Vector Double) -> p
  extractFromLevmarResult :: V.Vector Double -> p
  
instance HyperParams Double where
  extractFromVec v = (v ! 0)
  extractFromLevmarResult (V.toList -> [res_a]) = res_a
  
instance HyperParams (Double, Double) where
  extractFromVec v = ((v ! 0), (v ! 1))
  extractFromLevmarResult (V.toList -> [res_a, res_b]) = (res_a, res_b)
  
instance HyperParams (Double, Double, Double) where
  extractFromVec v = ((v ! 0), (v ! 1), (v ! 2))
  extractFromLevmarResult (V.toList -> [res_a, res_b, res_c]) = (res_a, res_b, res_c)

instance HyperParams (Double, Double, Double, Double) where
  extractFromVec v = ((v ! 0), (v ! 1), (v ! 2), (v ! 3))
  extractFromLevmarResult (V.toList -> [res_a, res_b, res_c, res_d]) = (res_a, res_b, res_c, res_d)
  
optimizeModel :: HyperParams hp => (hp -> Double -> Double) -> [(Double, Double)] -> [Double] -> (Double -> Double, hp, Loss)
optimizeModel f trainingData initialParams =
  let 
    xs = map fst trainingData
    ys = map snd trainingData
    result = 
      levmar 
        (\v -> V.fromList [f (extractFromVec v) x | x <- xs])
         Nothing
        (V.fromList initialParams)
        (V.fromList ys)
        50 -- Max iterations
        defaultOpts
        (Constraints Nothing Nothing Nothing Nothing)
    Right(res_v, _, _) = result
    res_hp = extractFromLevmarResult res_v
  in (f res_hp, res_hp, loss (f res_hp) trainingData)
 

    
      
