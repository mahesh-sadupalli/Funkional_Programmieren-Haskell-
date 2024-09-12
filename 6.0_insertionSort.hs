
insertionSort :: Ord a => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)


insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert a (x:xs) = 
    if a <= x then a : x : xs
    else x : insert a xs 



