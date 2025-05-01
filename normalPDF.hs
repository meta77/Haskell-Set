import Data.Function (on)
import Data.List (maximumBy)
import Numeric (showFFloat)

-- 正規分布の確率密度関数 (PDF)
normalPDF :: Floating a => a -> a -> a -> a
normalPDF mu sigma x =
  (1 / (sigma * sqrt (2 * pi))) *
  exp ( - (x - mu) ^ 2 / (2 * sigma ^ 2))

-- 例：平均0、標準偏差1の正規分布でx=1のときの確率密度
example1 :: Double
example1 = normalPDF 0 1 1

-- 正規分布のグラフ用データを作る
normalDistributionTable :: Floating a => a -> a -> [a] -> [(a, a)]
normalDistributionTable mu sigma xs = [(x, normalPDF mu sigma x) | x <- xs]

-- 実行例
main :: IO ()
main = do
  let mu = 0
      sigma = 1
      xs = [-3, -2.5..3] -- -3から3まで0.5刻み
      table = normalDistributionTable mu sigma xs
  putStrLn "x      f(x)"
  mapM_ (\(x, fx) -> putStrLn $ showFFloat (Just 2) x "" ++ "  " ++ showFFloat (Just 4) fx "") table
