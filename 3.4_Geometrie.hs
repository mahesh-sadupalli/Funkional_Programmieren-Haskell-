{-Algorithmieren und Programmieren SoSe'24: Übungsblatt03

Aufgabe4: Geometrische Funktion für Eine Gerade-}

--4(1) Tutoriumsaufgabe: Datentypen von Geraden
data Gerade = Gerade Double Double deriving (Show)
data Punkt = Punkt Double Double deriving (Show)

-- 4(2)Auswerten funktion von einer Gerade bei gegebenem x-Wert
auswerten :: Gerade -> Double -> Double
auswerten (Gerade m n) x = m * x + n

-- 4(3)Schnittpunkt funktion
schnittpunkt :: Gerade -> Gerade -> Punkt
schnittpunkt (Gerade m1 n1) (Gerade m2 n2)
  | m1 == m2 = error "Keinen Schnittpunkt, weil die Geraden sind Parallel."
  | otherwise = let x = (n2 - n1) / (m1 - m2)
                    y = auswerten (Gerade m1 n1) x
                in Punkt x y

-- 4(4)flaecheZwischenGeraden funktion im Intervall [a, b]
flaecheZwischenGeraden :: Gerade -> Gerade -> Double -> Double -> Double

--Schrittweite für die numerische Integration
--Die Wahl der Schrittweite von 0.001 bietet einen Kompromiss zwischen Genauigkeit und Leistung

flaecheZwischenGeraden g1 g2 a b = 
    let schrittweite = 0.001
        xs = [a, a+schrittweite .. b]
        flaeche x = abs (auswerten g1 x - auswerten g2 x) * schrittweite
    in sum (map flaeche xs)