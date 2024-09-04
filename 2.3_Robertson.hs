robertson :: Integer -> Integer -> Integer -> Integer 
robertson t m j = w where 
    a = m + 10
    b = ((m-14) `quot` 12 ) + j
    c = a - (12 *(a `quot`13))
    d = ((13*c) -1) `quot` 5
    e = (5*(b `mod` 100)) `quot` 4
    w = (d + t + 77 + e + (b `quot`400) - 2*(b `quot` 100)) `mod` 7

wochentag :: Integer -> String 
wochentag w = case w of 
    0 -> "Sonntag"
    1 -> "Montag"
    2 -> "Dienstag"
    3 -> "Mittwoch"
    4 -> "Donnerstag"
    5 -> "Freitag"
    6 -> "Samstag"

