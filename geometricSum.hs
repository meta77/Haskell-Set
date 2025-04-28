-- 等比数列の有限和を計算する
geometricSum :: Double -> Double -> Int -> Double
geometricSum a1 r n = a1 * (1 - r^n) / (1 - r)

-- 等比数列の無限和を計算する
-- rの絶対値が1未満が条件
geometricSumInfinite :: Double -> Double -> Maybe Double
geometricSumInfinite a1 r
    | abs r < 1 = Just (a1 / (1 - r))
    | otherwise = Nothing -- 収束しない場合

main :: IO ()
main = do
    -- 例1: 初項2、公比0.5、5項までの有限和
    let a1 = 2
    let r = 0.5
    let n = 5
    print $ "有限和 (初項2, 公比0.5, 5項): " ++ show (geometricSum a1 r n)

    -- 例2: 初項2、公比0.5の無限和
    case geometricSumInfinite a1 r of
        Just s  -> putStrLn $ "無限和 (初項2, 公比0.5): " ++ show s
        Nothing -> putStrLn "無限和は収束しません（rが1以上）"

    -- 例3: 初項1、公比2 の無限和（収束しないパターン）
    case geometricSumInfinite 1 2 of
        Just s  -> putStrLn $ "無限和 (初項1, 公比2): " ++ show s
        Nothing -> putStrLn "無限和は収束しません（rが1以上）"
