# Interestelar---Parcial-Funcional---Mi-rcoles-Tarde-2015

INTERESTELAR
En un futuro en el cual la tierra ya no es un buen lugar para habitar, las expediciones desesperadas por el universo en busca de nuevos planetas habitables llevó a la NASA a pedirnos que hagamos un programa para modelar cómo se ven afectados los astronautas por sus misiones. Tenemos los siguientes datos para trabajar:

-- Cada planeta tiene un nombre, una posición en el espacio y una relación que indica a cuánto tiempo terrestre equivale pasar un año allí.
data Planeta = UnPlaneta String Posicion (Int -> Int)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están
data Astronauta = UnAstronauta String Int Planeta

nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p

Desarrollar los siguientes requerimientos en Haskell usando tanto como sea posible: orden superior, aplicación parcial y composición.

1a) Saber la distancia entre 2 planetas sabiendo que la distancia entre dos posiciones se calcula de esta forma:

1b) Saber cuánto tiempo se tarda en viajar de un planeta a otro yendo a una determinada velocidad, que es la distancia entre ambos dividido por la velocidad de viaje.

2) Hacer una función pasarTiempo que haga que un astronauta pase una determinada cantidad de años en su planeta actual. Debería aumentar su edad terrestre en la cantidad de tiempo que el planeta indique a partir de los años indicados.

3) Queremos que un astronauta pueda viajar a otro planeta usando una nave determinada. Una nave es una función que dados dos planetas (origen y destino) retorna el tiempo requerido para viajar entre ellos. En principio nos interesa modelar las siguientes naves:
La nave vieja que cuya velocidad es 7 m/s a menos que tenga menos de 6 tanques de oxígeno, en cuyo caso viaja a 10 m/s.
La nave futurista que viaja tan rápido que el tiempo de viaje es despreciable.
Realizar un viaje implica que el astronauta aumente su edad en el tiempo de viaje correspondiente para llegar al destino elegido y cambie de planeta al mismo.

4a) Hacer que un grupo de astronautas rescate a un astronauta que quedó varado en otro planeta usando una determinada nave. Lo que se espera como resultado de efectuar un rescate es la lista de astronautas luego de que todos los rescatistas viajen en la nave a buscar al astronauta al planeta donde está varado, incorporen a la tripulación al rescatado tras pasar el tiempo correspondiente en ese planeta y luego viajen todos en la misma nave al planeta de donde vinieron los rescatistas.
Se puede asumir que todos venían del mismo planeta origen, y el tiempo que tiene que esperar el astronauta a rescatar es el que tarda la nave en ir de un lado a otro.

4b) Hacer una función que permita conocer dado un grupo de astronautas rescatistas, la nave que usan y un grupo de astronautas que quedaron varados en otros planetas, los nombres de los astronautas varados que podrían ser rescatados. Un astronauta puede ser rescatado por los rescatistas si luego del rescate ninguno de los astronautas (incluyendo al rescatado) tiene más de 90 años.

5) Inferir el tipo de la siguiente función:
f a b c = any ((> b).a b).filter (c 10)
