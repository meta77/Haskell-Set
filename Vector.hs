-- 2次元ベクトル
data Vec2 = Vec2 Double Double
  deriving (Show, Eq)

-- ベクトル同士の加算
addVec2 :: Vec2 -> Vec2 -> Vec2
addVec2 (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

-- スカラー倍
scaleVec2 :: Double -> Vec2 -> Vec2
scaleVec2 k (Vec2 x y) = Vec2 (k * x) (k * y)
