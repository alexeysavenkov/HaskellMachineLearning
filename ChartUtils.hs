module ChartUtils where

 -- SVGFonts
import qualified Graphics.SVGFonts.ReadFont as F

  -- Diagrams
import Diagrams.Prelude
import Diagrams.Backend.Rasterific

-- Chart
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import Graphics.Rendering.Chart.Backend.Types

  -- JuicyPixels
import Codec.Picture

import Control.Monad (when)

-- Support code
-- --  

renderChart :: (Default r, ToRenderable r) 
       => DEnv Double -> EC r () -> Image PixelRGBA8
renderChart env ec
  = renderDia Rasterific (RasterificOptions (mkWidth width)) $
      fst $ runBackendR env (toRenderable (execEC ec))
  where
    (width, _) = envOutputSize env

-- We need to use a local font.
chartEnv :: IO (DEnv Double)
chartEnv 
  = do
      sansR  <- F.loadFont "SourceSansPro_R.svg"
      sansRB <- F.loadFont "SourceSansPro_RB.svg"
      let fontSelect fs 
            = case (_font_name fs, _font_slant fs, _font_weight fs) of
                ("sans-serif", FontSlantNormal , FontWeightNormal) -> sansR
                ("sans-serif", FontSlantNormal , FontWeightBold  ) -> sansRB
      return $ createEnv vectorAlignmentFns 640 640 fontSelect
      
fSamples :: (Double -> Double) -> [Double] -> [(Double, Double)]
fSamples f xs 
  = [ (x, f x) 
    | x <- xs ]
    
-- | Construct a scatter plot with the given title and data, using the
-- next available color and point shape.
customPoints :: String -> [(x,y)] -> Double -> EC l (PlotPoints x y)
customPoints title values radius = liftEC $ do
    color <- takeColor
    shape <- takeShape
    plot_points_values .= values
    plot_points_title .= title
    plot_points_style . point_color .= color
    plot_points_style . point_shape .= shape
    plot_points_style . point_radius .= radius

    
plotFunction f label range = do plot (customPoints label (fSamples f range) 2)
    
amplitudeModulation
  = do
      layout_title .= "Amplitude Modulation"
      plotFunction (\x -> x*x*x) "kek" [0,(0.5)..400]
      --plot (points "am points" (signal [0,7..400]))
      
