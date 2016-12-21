
module Draw where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

coords :: [Double] -> [(Double, Double)]
coords = zip [0..]

-- | Plot and save a line graph
plotLine :: String -> [Double] -> IO ()
plotLine f d = toFile def f $ plot (line "l" [coords d])

-- | Count the letter frequencies
count :: String -> [Int]
count s = (\c -> length $ filter (c==) s) `map` ['A'..'Z']

-- | Create and save a frequency graph of a given string
freqGraph :: String -> String -> IO ()
freqGraph file ctext = plotLine file (map fromIntegral.count $ ctext)
