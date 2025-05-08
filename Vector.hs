-- 2次元ベクトル
data Vec2 = Vec2 Double Double
  deriving (Show, Eq)

-- ベクトル同士の加算
addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

-- スカラー倍
scaleVec2 :: Double -> Vec2 -> Vec2
scaleVec2 k (Vec2 x y) = Vec2 (k * x) (k * y)

-- ベクトルの例
v1 = Vec2 1.0 2.0
v2 = Vec2 3.0 4.0

-- ベクトル加算
sumVec = addVec2 v1 v2
-- Vec2 4.0 6.0

-- スカラー倍
scaledVec = scaleVec2 2.5 v1
-- Vec2 2.5 5.0


class VectorSpace v where
  zeroV :: v          -- ゼロベクトル
  (^+^) :: v -> v -> v  -- ベクトル加算
  (*^)  :: Double -> v -> v -- スカラー倍

instance VectorSpace Vec2 where
  zeroV = Vec2 0 0
  (Vec2 x1 y1) ^+^ (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)
  k *^ (Vec2 x y) = Vec2 (k*x) (k*y)

v1 = Vec2 1.0 2.0
v2 = Vec2 3.0 4.0

sumVec = v1 ^+^ v2
scaledVec = 3.0 *^ v1
