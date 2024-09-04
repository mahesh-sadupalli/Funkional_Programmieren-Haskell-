
g :: Double
g = 9.80665

fallstrecke :: Double -> Double 
fallstrecke t = (1/2) * g * t^2

fallstreckeWhere :: Double -> Double 
fallstreckeWhere t = (1/2) * g * t^2
    where
        g = 9.80665

fallstreckeLet :: Double -> Double 
fallstreckeLet t =
    let 
        g = 9.80665
    in 
        (1/2) * g * t^2

fallstreckeIf :: Double -> Double 
fallstreckeIf t = 
    if t > 0 then (1/2) * g * t^2 else error "negative" --where clause should apply to entire if ..then ...else not in between
    where 
        g = 9.80665

fallstreckeGuards :: Double -> Double 
fallstreckeGuards t 
    | t < 0 = error "Negative"
    | otherwise = (1/2) * g * t^2 where g = 9.80665

    


