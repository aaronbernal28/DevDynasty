-- Completar con los datos del grupo
--
-- Nombre de Grupo: DevDynasty
-- Integrante 1: Aaron Bernal Huanca, aaronbernal28@gmail.com, 815/22
-- Integrante 2: Nombre Apellido, email, LU
-- Integrante 3: Nombre Apellido, email, LU
-- Integrante 4: Nombre Apellido, email, LU

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

-- Nico y Nacho
-- describir qué hace la función: .....
nombresDeUsuarios :: RedSocial -> [String]
nombresDeUsuarios = undefined

-- describir qué hace la función: .....
amigosDe :: RedSocial -> Usuario -> [Usuario]
amigosDe = undefined

-- describir qué hace la función: .....
cantidadDeAmigos :: RedSocial -> Usuario -> Int
cantidadDeAmigos = undefined

-- describir qué hace la función: .....
usuarioConMasAmigos :: RedSocial -> Usuario
usuarioConMasAmigos = undefined

-- describir qué hace la función: .....
estaRobertoCarlos :: RedSocial -> Bool
estaRobertoCarlos = undefined

-- Aaron
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

-- describir qué hace la función:
-- devuelve true sii a los dos usuarios les gustaron las mismas publicaciones, comparo las dos listas
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

-- Agustin
-- devuelve true si algún usuario le dio "me gusta" a todas las publicaciones del usuario
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel rs us = likeATodasLasPubs (listaDeLikes rs us ) rs us

auxListaDeLikes :: [Publicacion] -> [Usuario] -> [Usuario]
auxListaDeLikes [] listaLikes = listaLikes
auxListaDeLikes pub listaLikes = auxListaDeLikes (tail pub) (likesDePublicacion (head pub)++listaLikes)

--devuelve la lista de usuarios que le dieron "me gusta" a las publicaciones del usuario que se ingresa por parámetro
listaDeLikes :: RedSocial -> Usuario -> [Usuario] 
listaDeLikes rs us = auxListaDeLikes (publicacionesDe rs  us) []

likeATodasLasPubs :: [Usuario] -> RedSocial -> Usuario -> Bool

likeATodasLasPubs us ([], rel, posts) usuario = False

likeATodasLasPubs usersDieronLike (users, rel, posts) usuario 
 |publicacionesDe (users, rel, posts) usuario == [] || cantidadDeApariciones usuario usersDieronLike == longitud (publicacionesDe (users, rel, posts) usuario) = False
 |cantidadDeApariciones (head users) usersDieronLike == longitud (publicacionesDe (users, rel, posts) usuario) = True
 |otherwise = likeATodasLasPubs usersDieronLike ((tail users),rel,posts) usuario

cantidadDeApariciones :: (Eq t) => t -> [t] -> Integer
cantidadDeApariciones e [] = 0
cantidadDeApariciones e lista |head lista == e = 1 + cantidadDeApariciones e (tail(lista))
                              |otherwise = cantidadDeApariciones e (tail(lista))

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- determina si existe una lista de usuarios que son amigos el primero con el segundo, el segundo con el tercero y así sucesivamente hasta el final de la lista. Dicha lista comienza con el primer usuario ingresado como parámetro y termina con el segundo.
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos ([],rel,posts) us1 us2 = False
existeSecuenciaDeAmigos ([x],rel,posts) us1 us2 = False
existeSecuenciaDeAmigos (users,rel,posts) us1 us2 = auxExisteSecuenciaDeAmigos users [] users (users,rel,posts) us1 us2

auxExisteSecuenciaDeAmigos :: [Usuario] ->  [Usuario] -> [Usuario] ->RedSocial -> Usuario -> Usuario -> Bool

auxExisteSecuenciaDeAmigos u1 lista users red us1 us2 |cadenaDeAmigos (us1:[us2]) red = True

                                                |cadenaDeAmigos lista red && empiezaCon us1 lista && terminaCon us2 lista = True

                                                |longitud lista < 1 && not(pertenece us1 lista) = auxExisteSecuenciaDeAmigos u1 (us1:lista) users red us1 us2

                                                |pertenece us1 lista && longitud lista < 2 && u1/=[] && cadenaDeAmigos [head u1,us1] red = auxExisteSecuenciaDeAmigos (tail u1) (lista++[head u1]) users red us1 us2

                                                |cadenaDeAmigos lista red && empiezaCon us1 lista && cadenaDeAmigos [elUltimo lista, us2] red = auxExisteSecuenciaDeAmigos u1 (lista++[us2]) users red us1 us2

                                                |longitud lista >= 2 && u1/=[] && cadenaDeAmigos [head u1, elUltimo lista] red= auxExisteSecuenciaDeAmigos (tail u1) (lista++[head u1]) users red us1 us2

                                                |u1 == [] && users/=[] && longitud lista >=2 && not(cadenaDeAmigos [elUltimo lista,us2] red) && not(pertenece (head users) lista) && cadenaDeAmigos [elUltimo lista, head users] red = auxExisteSecuenciaDeAmigos u1 (lista++[head users]) (tail users) red us1 us2

                                                |u1 == [] && users/=[] && longitud lista >=2 && not(cadenaDeAmigos [elUltimo lista,us2] red) && not(pertenece (head users) lista) && not(cadenaDeAmigos [elUltimo lista, head users] red) = auxExisteSecuenciaDeAmigos u1 lista (tail users) red us1 us2

                                                |u1 == [] && users/=[] && longitud lista >=2 && not(cadenaDeAmigos [elUltimo lista,us2] red) && pertenece (head users) lista = auxExisteSecuenciaDeAmigos u1 lista (tail users) red us1 us2

                                                |u1==[] && users==[] && not(cadenaDeAmigos lista red && empiezaCon us1 lista && terminaCon us2 lista) = False

                                                |otherwise = auxExisteSecuenciaDeAmigos (tail u1) lista users red us1 us2

-- Predicados Auxiliares

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e (x:xs) = e == x || pertenece e xs

mismosElementos :: (Eq t) => [t] -> [t] -> Bool
mismosElementos xs ys = incluido xs ys && incluido ys xs

incluido :: (Eq t) => [t] -> [t] -> Bool
incluido [] ys = True
incluido (x:xs) ys = pertenece x ys && incluido xs ys

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos = undefined

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto = undefined

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed red [] = True
sonDeLaRed red (u:us) = pertenece u (usuarios red) && sonDeLaRed red us

empiezaCon :: (Eq t) => t -> [t] -> Bool
empiezaCon e ls = ls /= [] && head ls == e

terminaCon :: (Eq t) => t -> [t] -> Bool
terminaCon e ls = elUltimo ls == e

elUltimo :: [t] -> t
elUltimo [x] = x
elUltimo xs = elUltimo (tail xs)

sinRepetidos :: (Eq t) => [t] -> Bool
sinRepetidos [] = True
sinRepetidos (x:xs) = not (pertenece x xs) && sinRepetidos xs


