
--Übung 01: Aufgabe 04- Haskell Funktionskomposition

--4(1) Funktionen von Pyramidenmaßnahmen

--Grundfläche 'g' mit der Seitenlänge 'a'
grundflaecheAusSeitenlaenge :: Double -> Double
grundflaecheAusSeitenlaenge a = a * a

--Seitenlänge 'a' einer quadratischen Grundfläche 'g'
seitenlaengeAusGrundflaeche :: Double -> Double
seitenlaengeAusGrundflaeche g = sqrt g


-- das Volumen mit der Grundfläche 'g' und der Höhe 'h'

volumenAusGrundflaecheUndHoehe :: Double -> Double -> Double
volumenAusGrundflaecheUndHoehe g h = (g * h) / 3

--Grundfläche einer Pyramide mit dem Volumen 'v' und der Höhe 'h'

grundflaecheAusVolumenUndHoehe :: Double -> Double -> Double
grundflaecheAusVolumenUndHoehe v h = (3 * v) / h

--die Höhe einer Pyramide mit der Grundfläche 'g' und dem Volumen 'v'

hoeheAusVolumenUndGrundflaeche :: Double -> Double -> Double
hoeheAusVolumenUndGrundflaeche v g = (3 * v) / g

--die Seitenhöhe einer Pyramide mit der zentralen Höhe 'h' und der Seitenlänge 'a'

seitenhoeheAusHoeheUndSeitenlaenge :: Double -> Double -> Double
seitenhoeheAusHoeheUndSeitenlaenge h a = sqrt(h * h + (a / 2) * (a / 2))

-- Die Mantelfläche einer Pyramide mit der Seitenlänge 'a' und der Seitenhöhe 's'

mantelflaecheAusSeitenlaengeUndHoehe :: Double -> Double -> Double
mantelflaecheAusSeitenlaengeUndHoehe a s = a * s * 4

-- Die Oberfläche einer Pyramide mit der Seitenlänge 'a' und der Höhe 'h'.

oberflaecheAusSeitenlaengeUndHoehe :: Double -> Double -> Double
oberflaecheAusSeitenlaengeUndHoehe a h =
  let grundflaeche = grundflaecheAusSeitenlaenge a
      seitenhoehe = seitenhoeheAusHoeheUndSeitenlaenge h a
      mantelflaeche = mantelflaecheAusSeitenlaengeUndHoehe a seitenhoehe
  in grundflaeche + mantelflaeche


--Gaurds
--grundflaecheAusSeitenlaenge
grundflaecheAusSeitenlaengeGuards :: Double -> Double 
grundflaecheAusSeitenlaengeGuards a | a < 0 = error "Negative zeit"
grundflaecheAusSeitenlaengeGuards a | otherwise = a * a 

--seitenlaengeAusGrundflaeche
seitenlaengeAusGrundflaecheGuards :: Double -> Double
seitenlaengeAusGrundflaecheGuards g |g < 0 = error "Negative zeit"
seitenlaengeAusGrundflaecheGuards g |otherwise = g * g

--volumenAusGrundflaecheUndHoehe
volumenAusGrundflaecheUndHoeheGuards :: Double -> Double -> Double
volumenAusGrundflaecheUndHoeheGuards g h
    |g < 0 || h < 0 = error "Negative zeit"
    |otherwise = (g * h) / 3


--grundflaecheAusVolumenUndHoehe

grundflaecheAusVolumenUndHoeheGuards :: Double -> Double -> Double
grundflaecheAusVolumenUndHoeheGuards v h
    |v < 0 || h < 0 = error "Negative zeit"
    |otherwise = (3 * v) / h

--hoeheAusVolumenUndGrundflaeche

hoeheAusVolumenUndGrundflaecheGuards :: Double -> Double -> Double
hoeheAusVolumenUndGrundflaecheGuards v g
    |v < 0 || g < 0 = error "Negative zeit"
    |otherwise = (3 * v) / g

--seitenhoeheAusHoeheUndSeitenlaenge

seitenhoeheAusHoeheUndSeitenlaengeGuards :: Double -> Double -> Double
seitenhoeheAusHoeheUndSeitenlaengeGuards h a 
    |h < 0 || a < 0 = error "Negative zeit"
    |otherwise = sqrt(h * h + (a / 2) * (a / 2))

--mantelflaecheAusSeitenlaengeUndHoehe

mantelflaecheAusSeitenlaengeUndHoeheGuards :: Double -> Double -> Double
mantelflaecheAusSeitenlaengeUndHoeheGuards a s 
    |a < 0 || s < 0 = error "Negative zeit"
    |otherwise = a * s * 4

-- oberflaecheAusSeitenlaengeUndHoehe

oberflaecheAusSeitenlaengeUndHoeheGuards :: Double -> Double -> Double
oberflaecheAusSeitenlaengeUndHoeheGuards a h
    |a < 0 || h < 0 = error "Negative zeit"
    |otherwise = 
        let grundflaeche = grundflaecheAusSeitenlaenge a
            seitenhoehe = seitenhoeheAusHoeheUndSeitenlaenge h a
            mantelflaeche = mantelflaecheAusSeitenlaengeUndHoehe a seitenhoehe
        in grundflaeche + mantelflaeche