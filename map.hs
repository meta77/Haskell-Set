-- 写像のデータ型
data Mapping a b = Mapping {
  apply :: a -> b,    -- 写像の本体 (関数)
  domain :: [a],      -- 定義域 (A)
  codomain :: [b]     -- 値域 (B)
}