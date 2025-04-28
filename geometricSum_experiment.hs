-- 等比数列をリストとして生成する関数
geometricList :: Double -> Double -> Int -> [Double]
geometricList a1 r n = take n [a1 * r^i | i <- [0..]]

{-
[a1 * r^i | i <- [0..]]の解説

i <- [0..]
これは「リスト [0..] から順番に要素を取り出して、それを i という名前で使う」という意味です。
内包表記の中の生成部分と呼ばれます。

リストモナドの「本質」は：
「たくさんの値を持っていて、それぞれについて計算を進める」

doでもかける
リストモナドのdo記法の省略形と考えられる！

do
  i <- [0..]
  return (a1 * r^i)

[0..] から「i」を1個ずつ取り出して
return (a1 * r^i) をして
全部集めて新しいリストにする

このdo表記は、このようにもかける。
[0..] >>= (\i -> return (a1 * r^i))

リストモナドでは、「すべての要素に適用して、結果を連結する」
これが「リストという文脈」での動き方です。
他のモナドなら、動きが変わります。

>>= 自体は「モナドの一般的な型 (a -> m b) を a から b へつなぐもの」
でも「具体的な動き方」はモナドごとに違う
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
