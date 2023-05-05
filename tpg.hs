-- Ejercicios 
-- describir qué hace la función: 
-- Toma una RedSocial (red) y devuelve una lista de los nombres de todos los Usuarios de la red
-- requiere red social valida, es decir no hay usuarios repetidos, sin embargo
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = eliminarRepetidos (proyectarNombres (usuarios red))

quitartodos ::(Eq t) => t -> [t] -> [t] 
quitartodos n [] = []
quitartodos n (x:xs) |n == x = quitartodos n xs
                     |otherwise = x : (quitartodos n xs)

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = [] 
eliminarRepetidos (x:xs) = x : quitartodos x (eliminarRepetidos xs)

proyectarNombres :: [Usuario] -> [String]  
proyectarNombres [] = []
proyectarNombres (u:us) = nombreDeUsuario u : eliminarRepetidos (proyectarNombres us)


-- describir qué hace la función:
-- toma un usuario (u) de la RedSocial (red) y devuelve una lista de todos los usuarios (u) con los cuales se relaciona
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeAux (relaciones red) u

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] u = []
amigosDeAux (rel:rels) u
    | estaRelacionado rel u = laRelacion rel u : amigosDeAux rels u
    | otherwise = amigosDeAux rels u

estaRelacionado :: Relacion -> Usuario -> Bool
estaRelacionado (x,y) u = x == u || y == u

laRelacion :: Relacion -> Usuario -> Usuario
laRelacion (x,y) u
    | x == u = y
    | otherwise = x

-- describir qué hace la función: 
-- toma un usuario (u) de la RedSocial (red) y devuelve la cantidad de amigosDe del usuario (u)
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos red u = longitud (amigosDe red u)

longitud :: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- describir qué hace la función: 
-- toma lista RedSocial (red) y devuelve el usuario con la mayor cantidad de amigos
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos red = usuarioConMasAmigosAux red (usuarios red)

usuarioConMasAmigosAux :: RedSocial -> [Usuario] -> Usuario
usuarioConMasAmigosAux red [u] = u
usuarioConMasAmigosAux red (u1:u2:us)
    | cantidadDeAmigos red u1 >= cantidadDeAmigos red u2 = usuarioConMasAmigosAux red (u1:us)
    | otherwise = usuarioConMasAmigosAux red (u2:us)

-- describir qué hace la función: 
-- si un usuario pertenece a RedSocial (red) y su cantidadDeAmigos es mayor a 1.000.000 entonces es cierto
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos red = cantidadDeAmigos red (usuarioConMasAmigos red) > 1000000