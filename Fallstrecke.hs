--Aufgabe 3(Einfache Haskell Funcktionen)

--3(1&2)Datentypen: Großen Weg, zeit und Beschleunigung und Fallstrecke(1)= 4.903325
fallstrecke :: Double -> Double
fallstrecke t = (9.80665 * t * t)/2

--3(3) FallstreckeGlobal, dass g als globale Variable deklariert 
g :: Double
g = 9.80665

fallstreckeGlobal :: Double -> Double
fallstreckeGlobal t = (g * t * t)/2

--3(4) fallstreckeWhere 

fallstreckeWhere :: Double -> Double 
fallstreckeWhere t = 
    1/2 * g * t^2
    where
        g = 9.80665


--3(5) let-Konstruct: FallstreckeLet

fallstreckeLet :: Double -> Double 
fallstreckeLet t = 
    let
        g = 9.80665
    in 
        1/2 * g * t^2


--Funktion mit negative wert: fallstreckeIf

fallstreckeIf :: Double -> Double
fallstreckeIf t =
    if t < 0 then error "Negative zeit" 
    else 
        1/2 * g * t
        where
            g = 9.80665


--fallstrckeGaurds

fallstreckeGuards :: Double -> Double 
fallstreckeGuards t | t < 0 = error "Negative zeit"
fallstreckeGuards t | otherwise = 1/2 * g * t^2 where g = 9.80665