-- 写像のデータ型
data Mapping a b = Mapping {
  apply :: a -> b,    -- 写像の本体 (関数)
  domain :: [a],      -- 定義域 (A)
  codomain :: [b]     -- 値域 (B)
}

import Data.List (nub, sort)

-- 単射判定
isInjective :: (Eq b) => Mapping a b -> Bool
isInjective (Mapping f dom _) =
  let images = map f dom
  in length (nub images) == length images

-- 全射判定
isSurjective :: (Eq b) => Mapping a b -> Bool
isSurjective (Mapping f dom codom) =
  let images = map f dom
  in all (`elem` images) codom

-- 全単射判定
isBijective :: (Eq b) => Mapping a b -> Bool
isBijective m = isInjective m && isSurjective m
