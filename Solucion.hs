module Solucion where

-- Nombre de Grupo: DevDynasty
-- Integrante 1: Aaron Bernal Huanca
-- Integrante 2: Agustín Benedicto Perez Cometto

type Usuario = (Integer, String) -- (id, nombre)
type Relacion = (Usuario, Usuario) -- usuarios que se relacionan
type Publicacion = (Usuario, String, [Usuario]) -- (usuario que publica, texto publicacion, likes)
type RedSocial = ([Usuario], [Relacion], [Publicacion])

-- Funciones basicas

usuarios :: RedSocial -> [Usuario]
usuarios (us, _, _) = us

relaciones :: RedSocial -> [Relacion]
relaciones (_, rs, _) = rs

publicaciones :: RedSocial -> [Publicacion]
publicaciones (_, _, ps) = ps

idDeUsuario :: Usuario -> Integer
idDeUsuario (id, _) = id 

nombreDeUsuario :: Usuario -> String
nombreDeUsuario (_, nombre) = nombre 

usuarioDePublicacion :: Publicacion -> Usuario
usuarioDePublicacion (u, _, _) = u

likesDePublicacion :: Publicacion -> [Usuario]
likesDePublicacion (_, _, us) = us

-- Ejercicios 

-- describir qué hace la función: 
-- Toma una RedSocial (red) y devuelve una lista de los nombres de todos los Usuarios de la red

nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios red = proyectarNombres (usuarios red) --no hace falta eliminarRepetidos

proyectarNombres :: [Usuario] -> [String]
proyectarNombres us = eliminarRepetidos (proyectarNombresAux us)

proyectarNombresAux :: [Usuario] -> [String]
proyectarNombresAux [] = []
proyectarNombresAux (u:us) = nombreDeUsuario u : proyectarNombresAux us

eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = [] 
eliminarRepetidos (x:xs) = x : quitarTodos x (eliminarRepetidos xs)

quitarTodos ::(Eq t) => t -> [t] -> [t] 
quitarTodos n [] = []
quitarTodos n (x:xs)
    | n == x = quitarTodos n xs
    | otherwise = x : (quitarTodos n xs)

-- describir qué hace la función:
-- toma un usuario (u) de la RedSocial (red) y devuelve una lista de todos los usuarios (u) con los cuales se relaciona

amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe red u = amigosDeAux (relaciones red) u

amigosDeAux :: [Relacion] -> Usuario -> [Usuario]
amigosDeAux [] u = []
amigosDeAux (rel:rels) u
    | estaRelacionado rel u = elAmigo rel u : amigosDeAux rels u
    | otherwise = amigosDeAux rels u

estaRelacionado :: Relacion -> Usuario -> Bool
estaRelacionado (u1,u2) u = u1 == u || u2 == u

elAmigo :: Relacion -> Usuario -> Usuario
elAmigo (u1,u2) u
    | u1 == u = u2
    | otherwise = u1

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

-- describir qué hace la función:
-- toma la lista de publicaciones de RedSocial (red) y devuelve las que fueron creadas por usuario (u)

publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe red u = publicacionesDeAux (publicaciones red) u

publicacionesDeAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesDeAux [] u = []
publicacionesDeAux (pub:pubs) u
    | usuarioDePublicacion pub == u = pub : publicacionesDeAux pubs u 
    | otherwise = publicacionesDeAux pubs u

-- describir qué hace la función:
-- toma la lista de publicaciones de red y devuelve las publicaciones que le gustaron a usuario (u)

publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA red u = publicacionesQueLeGustanAAux (publicaciones red) u

publicacionesQueLeGustanAAux :: [Publicacion] -> Usuario -> [Publicacion]
publicacionesQueLeGustanAAux [] u = []
publicacionesQueLeGustanAAux (pub:pubs) u
    | pertenece u (likesDePublicacion pub) = pub : publicacionesQueLeGustanAAux pubs u
    | otherwise = publicacionesQueLeGustanAAux pubs u

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

-- describir qué hace la función:
-- devuelve true sii a los dos usuarios les gustaron las mismas publicaciones, comparo las dos listas

lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = incluido xs ys && incluido ys xs

incluido :: (Eq t) => [t] -> [t] -> Bool
incluido [] ys = True
incluido (x:xs) ys = pertenece x ys && incluido xs ys

-- describir qué hace la función:
-- devuelve true si las publicaciones que les gustaron a algún usuario (u1) contiene a todas las publicaciones del usuario en cuestion

tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel red u
    | longitud (publicacionesDe red u) == 0 = False
    | otherwise = tieneUnSeguidorFielAux red (usuarios red) u

tieneUnSeguidorFielAux :: RedSocial -> [Usuario] -> Usuario -> Bool
tieneUnSeguidorFielAux red [] u = False
tieneUnSeguidorFielAux red (u2:us) u
    | u2 == u = tieneUnSeguidorFielAux red us u -- usuario no puede ser su propio seguidoFiel
    | incluido (publicacionesDe red u) (publicacionesQueLeGustanA red u2) = True
    | otherwise = tieneUnSeguidorFielAux red us u

-- describir qué hace la función:
-- supongamos que existe una secuencia de amigos entre u1 y u2
-- entonces existe una secuencia de amigos entre 'alguno de los amigos de u1' y 'del mismo u2', en existeSecuenciaDeAmigos
-- se van a crear ramificaciones de amistad que finalizan cuando se encuentra a u2 o cuando no hay mas amigos de amigos que ver

existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos red u1 u2 | u1 == u2 && amigosDe red u1 /= [] = True
existeSecuenciaDeAmigos red u1 u2 = relacionadosDirecto u1 u2 red || existeSecuenciaDeAmigosAux (sacarLasRelacionesDe red u1) (amigosDe red u1) u2

existeSecuenciaDeAmigosAux :: RedSocial -> [Usuario] -> Usuario -> Bool
existeSecuenciaDeAmigosAux red [] u2 = False
existeSecuenciaDeAmigosAux red (u:us) u2 = existeSecuenciaDeAmigos red u u2 || existeSecuenciaDeAmigosAux red us u2

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto u1 u2 red = pertenece (u1,u2) (relaciones red) || pertenece (u2,u1) (relaciones red)

-- elimino las relaciones de u1 para no volver a pasar por ellas

sacarLasRelacionesDe :: RedSocial -> Usuario -> RedSocial
sacarLasRelacionesDe red u1 = (usuarios red, sacarLasRelacionesDeAux (relaciones red) u1, publicaciones red)

sacarLasRelacionesDeAux :: [Relacion] -> Usuario -> [Relacion]
sacarLasRelacionesDeAux [] u1 = []
sacarLasRelacionesDeAux (rel:rels) u1
    | estaRelacionado rel u1 = sacarLasRelacionesDeAux rels u1
    | otherwise = rel : sacarLasRelacionesDeAux rels u1


