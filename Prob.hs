type PMF a = [(a, Double)]  -- 値とその確率

dicePMF :: PMF Int
dicePMF = [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]

pmf :: Eq a => PMF a -> a -> Double
pmf table x = sum [p | (x', p) <- table, x == x']

cdf :: (Ord a) => PMF a -> a -> Double
cdf table x = sum [p | (x', p) <- table, x' <= x]

{-
[p | (x', p) <- table, x' <= x]の解説

[出力 | 入力 <- データ, 条件]
(x', p) <- table
table は [(a, Double)] 型のリスト

各要素 (x', p) に分解して取り出す
例：(2, 0.1) → x' = 2, p = 0.1

▼ x' <= x
条件付きで「x' が x 以下」ならその値を使う

▼ p
条件を満たした (x', p) のうち、p（確率）だけを取り出す

👉 結果：x' <= x を満たすすべての p（確率値）のリスト

do表記でもかける
do
  (x', p) <- table
  guard (x' <= x)
  return p

-}

main :: IO ()
main = do
    putStrLn $ "P(X = 3): " ++ show (pmf dicePMF 3)   -- => 0.166...
    putStrLn $ "P(X <= 3): " ++ show (cdf dicePMF 3)  -- => 0.5
