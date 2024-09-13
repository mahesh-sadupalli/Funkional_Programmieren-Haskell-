
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x: xs) = insert x (insertionSort xs)

insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) = 
    if a <= x then a : x : xs
    else x : (insert a xs) 


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    |otherwise = y: merge (x:xs) ys 

mergeSort :: Ord a => [a] -> Int -> [a]
mergeSort xs n
    | length xs <= n = insertionSort xs
    |otherwise = merge (mergeSort left n) (mergeSort right n)
    where 
        (left, right) = splitAt (length xs `div` 2 ) xs
    


