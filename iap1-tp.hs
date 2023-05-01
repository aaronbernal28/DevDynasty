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
    | pertence u (likesDePublicacion pub) = pub : publicacionesQueLeGustanAAux pubs u
    | otherwise = publicacionesQueLeGustanAAux pubs u

-- describir qué hace la función:
-- devuelve true sii a los dos usuarios les gustaron las mismas publicaciones, comparo las dos listas
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones red u1 u2 = mismosElementos (publicacionesQueLeGustanA red u1) (publicacionesQueLeGustanA red u2)

-- Agustin
-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

-- Predicados Auxiliares

pertence :: t -> [t] -> Bool
pertence = undefined

mismosElementos :: [t] -> [t] -> Bool
mismosElementos = undefined

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos = undefined

relacionadosDirecto :: Usuario -> Usuario -> RedSocial -> Bool
relacionadosDirecto = undefined

sonDeLaRed :: RedSocial -> [Usuario] -> Bool
sonDeLaRed = undefined

empiezaCon :: t -> [t] -> Bool
empiezaCon = undefined

terminaCon :: t -> [t] -> Bool
terminaCon = undefined

sinRepetidos :: [t] -> Bool
sinRepetidos = undefined

