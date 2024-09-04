und :: Bool -> Bool -> Bool
und = (&&)

nicht :: Bool -> Bool 
nicht = not

oder :: Bool -> Bool -> Bool 
oder a b = nicht(nicht a `und` nicht b)

darausFolgt :: Bool -> Bool -> Bool 
darausFolgt a b = nicht (a `und` nicht b)

genauDannWenn :: Bool -> Bool -> Bool 
genauDannWenn a b = und (darausFolgt a b) (darausFolgt b a)

entwederOder :: Bool -> Bool -> Bool
entwederOder a b = und (oder a b) (nicht (und a b))

