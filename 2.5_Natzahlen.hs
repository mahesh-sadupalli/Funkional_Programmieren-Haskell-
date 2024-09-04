plus:: Integer -> Integer -> Integer 
plus 0 a = a
plus a b = succ (plus (pred a) b)

mal:: Integer -> Integer -> Integer 
mal 0 _ = 0
mal 1 b = b
mal a b = plus b (mal (pred a) b)


potenz :: Integer -> Integer -> Integer 
potenz _ 0 = 1
potenz a 1 = a
potenz a b = mal a (potenz a (pred b))