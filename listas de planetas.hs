--Parte de la resolucion del Ejercicio Planetas (TP individual)

data Planeta = Planeta {nombreP :: String, posicionP :: (Float,Float,Float),
                          pAgua :: Float}  deriving Show


proximaCentauriB = Planeta {nombreP = "Proxima Centauri b",
                            posicionP = (43.2,14.2,8.9),
                            pAgua = 74}

alphaCentauriBb = Planeta {nombreP = "Alpha Centauri Bb",
                           posicionP = (17,31.2,32),
                           pAgua = 3}

alphaCentauriCc = Planeta {nombreP = "Alpha Centauri Cc",
                           posicionP = (42,42,42),
                           pAgua = 60}

data Naves = Nave {nombreNave :: String, cantComb :: Int, tripulantes :: [String], destino :: Planeta }

starDestroyer = Nave {nombreNave = "Venator", cantComb = 100, tripulantes = ["Appo","Rex","Eco","Fives","Tup"], destino = alphaCentauriBb}


-- Ponemos una posicion arbitraria para la nave
posicionNave = (0,0,0)

listaDePlanetas = [proximaCentauriB,alphaCentauriCc,alphaCentauriBb]

pri (x,_,_) = x
seg (_,y,_) = y
ter (_,_,z) = z

distanciaA (x0,y0,z0) planeta =  (x0 * (pri (posicionP planeta))) +
                                              (2*y0 *(ter (posicionP planeta))) + abs((seg (posicionP planeta))-z0)

planetaApto posicion planeta = (pAgua planeta > 52 ) && (distanciaA posicion planeta < 100)

cargarProximoPlaneta nave listaDePlanetas = nave {destino = head listaDePlanetas}


estaOrdenada [] _ = True
estaOrdenada [_] _ = True
estaOrdenada (planeta1:planeta2:planetas) posicion = (distanciaA posicion planeta1) <= (distanciaA posicion planeta2) && estaOrdenada (planeta2:planetas) posicion


cuantosPlanetasAptos posicion planetas = length (filter (planetaApto posicion) planetas)

--Solución sin aplicación parcial, asumiendo una posicion predeterminada
--distanciaALaNave planeta = distanciaA posicionNave planeta 
--mayorDistanciaPlanetas planetas = maximum (map distanciaALaNave  planetas)

--Solución con aplicación parcial
mayorDistanciaPlanetas planetas = maximum (map (distanciaA posicionNave) planetas)

--aumentarPorcentajeDeAgua :: Planeta -> Planeta
aumentarPorcentajeDeAgua porc planeta = planeta {pAgua = pAgua planeta * (1+porc)}

--aumentoAguaPlanetas planetas = map aumentarPorcentajeDeAgua planetas


--Aplicación parcial
triple = (3 *)

variasVeces x = 3 * x

--Función de orden superior
sumaEspecial criterio elemento1 elemento2 = criterio elemento1 + criterio elemento2

--Composición
longitudPar:: [Char]->Bool
longitudPar = even.length

--Tipos de datos variables
funcionInutil::a->a
funcionInutil x = x

--Función que genera lista infinita
f x = x*7:f (x +1)

--Sin expresiones lambda
-- map polinomio [1..10]
polinomio x = x + 2*x + 4 * x * x

--Con expresión lambda
--map (\x -> x + 2*x + 4 * x * x) [1..10]
