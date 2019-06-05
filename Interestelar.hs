data Planeta = UnPlaneta String Posicion (Int -> Int)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)

coordX (x, _, _) = x
coordY (_, y, _) = y
coordZ (_, _, z) = z

data Astronauta = UnAstronauta String Int Planeta

nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p

--1a)

distancia :: Posicion -> Posicion -> Float
distancia (x1, y1, z1) (x2, y2, z2) = 
  sqrt ((x1-x2)^2 + (y1-y2)^2 + (z1-z2)^2)

distanciaEntreDosPlanetas :: Planeta -> Planeta -> Float
distanciaEntreDosPlanetas planeta = 
  distancia (posicion planeta) . posicion

--1b)

tiempoDeViaje :: Planeta -> Float -> Planeta -> Float
tiempoDeViaje planetaO velocidad =
  (/ velocidad) . distanciaEntreDosPlanetas planetaO


--2)

pasarTiempo :: Astronauta -> Int -> Planeta -> Astronauta
pasarTiempo internauta anhos planetaActual =
  UnAstronauta (nombre internauta) 
  ((+ edad internauta).tiempo planetaActual $ anhos) 
  (planeta internauta)

--3)

