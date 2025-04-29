type PMF a = [(a, Double)]  -- 値とその確率

dicePMF :: PMF Int
dicePMF = [(1, 1/6), (2, 1/6), (3, 1/6), (4, 1/6), (5, 1/6), (6, 1/6)]

pmf :: Eq a => PMF a -> a -> Double
pmf table x = sum [p | (x', p) <- table, x == x']

cdf :: (Ord a) => PMF a -> a -> Double
cdf table x = sum [p | (x', p) <- table, x' <= x]

main :: IO ()
main = do
    putStrLn $ "P(X = 3): " ++ show (pmf dicePMF 3)   -- => 0.166...
    putStrLn $ "P(X <= 3): " ++ show (cdf dicePMF 3)  -- => 0.5
