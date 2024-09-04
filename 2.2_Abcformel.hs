abcformelLet :: Double -> Double -> Double -> (Double, Double)

abcformelLet a b c 
    | (b^2 - 4*a*c) < 0 = error "complex roots"
    | otherwise =
        let 
            root1 = (-b + sqrt (b^2 - 4*a*c) ) / (2*a)
            root2 = (-b - sqrt (b^2 - 4*a*c)) / (2*a)
        in 
            (root1, root2)


abcformelWhere :: Double -> Double -> Double -> (Double, Double)

abcformelWhere a b c 
    | (b^2 - 4*a*c) < 0 = error "complex roots"
    | otherwise = (root1, root2)
    where 
        root1 = (-b + sqrt (b^2 - 4*a*c) ) / (2*a)
        root2 = (-b - sqrt (b^2 - 4*a*c)) / (2*a)



