-- 等比数列をリストとして生成する関数
geometricList :: Double -> Double -> Int -> [Double]
geometricList a1 r n = take n [a1 * r^i | i <- [0..]]

{-
[a1 * r^i | i <- [0..]]の解説


-}

-- リストの和を求める
sumGeometricList :: Double -> Double -> Int -> Double
sumGeometricList a1 r n = sum (geometricList a1 r n)

-- 公式で有限和を計算する
geometricSumFormula :: Double -> Double -> Int -> Double
geometricSumFormula a1 r n = a1 * (1 - r^n) / (1 - r)

main :: IO ()
main = do
    let a1 = 2
    let r = 0.5
    let n = 5

    let listSum = sumGeometricList a1 r n
    let formulaSum = geometricSumFormula a1 r n

    putStrLn $ "リストから求めた和: " ++ show listSum
    putStrLn $ "公式から求めた和: " ++ show formulaSum

    -- 差が小さいか（誤差確認）
    let diff = abs (listSum - formulaSum)
    putStrLn $ "差（誤差）: " ++ show diff
