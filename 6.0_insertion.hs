insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)
    where
        insert y [] = [y]
        insert y (z:zs)
            |y <= z = y : z : zs
            |otherwise = z : insert y zs