
data Tree a = Empty | Node a (Tree a) (Tree a)

anzahlKnoten :: Tree a -> Int 
anzahlKnoten Empty = 0
anzahlKnoten (Node _ l r) = 1 + (anzahlKnoten l) + (anzahlKnoten r)

depth :: Tree a -> Int 
depth Empty = 0
depth (Node _ l r) = 1 + max (depth l)(depth r)

