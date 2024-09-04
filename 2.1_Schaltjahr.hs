schaltjahrIf :: Integer -> Bool
schaltjahrIf x 
    = if (x `mod`400) == 0 then 
        True 
    else 
        if x `mod` 100 == 0 then 
            False
    else 
        if x `mod` 4 == 0 then 
            True
    else 
        False 


schaltjahrGuards :: Integer -> Bool 
schaltjahrGuards x 
    | x `mod` 400 == 0 = True 
    | x `mod` 4 == 0 = True 
    | x `mod` 100 == 0 = False
    | otherwise = False 


schaltjahrBool :: Integer -> Bool
schaltjahrBool x = (x `mod` 400 == 0)  && ((x `mod` 100 /= 0)  || (x `mod` 4 == 0))