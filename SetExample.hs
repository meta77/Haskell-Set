-- 集合を定義するデータ型
-- 集合は重複のないリストとして扱う
module Main where

-- 集合型の定義
newtype Set a = Set [a] deriving (Show)

-- 集合を作る関数（重複を削除）
makeSet :: (Eq a) => [a] -> Set a
makeSet xs = Set (removeDuplicates xs)

-- 重複を取り除くヘルパー関数
removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) -- 再帰 により、空リスト [] に到達するまで処理が続く。空リスト [] に到達すると、逆順に結果を構築しながら戻っていく。
    | x `elem` xs = removeDuplicates xs
    | otherwise    = x : removeDuplicates xs

-- 要素を追加する関数
addElement :: (Eq a) => a -> Set a -> Set a
addElement x (Set xs)
    | x `elem` xs = Set xs  -- すでに存在するならそのまま
    | otherwise   = Set (x:xs)

-- 要素を削除する関数
removeElement :: (Eq a) => a -> Set a -> Set a
removeElement _ (Set []) = Set []
removeElement x (Set (y:ys))
    | x == y    = Set ys
    | otherwise = addElement y (removeElement x (Set ys)) -- 先頭の要素 y はそのままにして、再帰結果に再び追加します。削除対象でない要素は順に戻す。

-- 和集合 (Union)
union :: (Eq a) => Set a -> Set a -> Set a
union (Set xs) (Set ys) = makeSet (xs ++ ys)

-- 共通部分 (Intersection)
intersection :: (Eq a) => Set a -> Set a -> Set a
intersection (Set xs) (Set ys) = makeSet [x | x <- xs, x `elem` ys] -- x が ys に含まれているならリストに残す。

-- 集合の要素数を返す
setSize :: Set a -> Int
setSize (Set xs) = length xs

-- メイン関数
main :: IO ()
main = do
    let set1 = makeSet [1, 2, 3, 4]
    let set2 = makeSet [3, 4, 5, 6]

    putStrLn "Set 1:"
    print set1
    putStrLn "Set 2:"
    print set2

    -- 和集合
    let setUnion = union set1 set2
    putStrLn "\nUnion of Set 1 and Set 2:"
    print setUnion

    -- 共通部分
    let setIntersection = intersection set1 set2
    putStrLn "\nIntersection of Set 1 and Set 2:"
    print setIntersection

    -- 要素の追加
    let set3 = addElement 7 set1
    putStrLn "\nSet 1 after adding element 7:"
    print set3

    -- 要素の削除
    let set4 = removeElement 2 set1
    putStrLn "\nSet 1 after removing element 2:"
    print set4

    -- 集合のサイズ
    putStrLn "\nSize of Set 1:"
    print (setSize set1)
