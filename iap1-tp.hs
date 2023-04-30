-- Completar con los datos del grupo
--
-- Nombre de Grupo: DevDinasty
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
-- describir qué hace la función: .....
publicacionesDe :: RedSocial -> Usuario -> [Publicacion]
publicacionesDe = undefined

-- describir qué hace la función: .....
publicacionesQueLeGustanA :: RedSocial -> Usuario -> [Publicacion]
publicacionesQueLeGustanA = undefined

-- describir qué hace la función: .....
lesGustanLasMismasPublicaciones :: RedSocial -> Usuario -> Usuario -> Bool
lesGustanLasMismasPublicaciones = undefined

-- Agustin
-- describir qué hace la función: .....
tieneUnSeguidorFiel :: RedSocial -> Usuario -> Bool
tieneUnSeguidorFiel = undefined

-- describir qué hace la función: .....
existeSecuenciaDeAmigos :: RedSocial -> Usuario -> Usuario -> Bool
existeSecuenciaDeAmigos = undefined

-- Predicados Auxiliares

-- Nico y Nacho
pertence :: t -> [t] -> Bool
pertence = undefined

mismosElementos :: [t] -> [t] -> Bool
mismosElementos = undefined

redSocialValida :: RedSocial -> Bool
redSocialValida = undefined

usuariosValidos :: [Usuario] -> Bool
usuariosValidos = undefined

usuarioValido :: Usuario -> Bool
usuarioValido = undefined

noHayIdsRepetidos :: [Usuario] -> Bool
noHayIdsRepetidos = undefined

relacionesValidas :: [Usuario] -> [Relacion] -> Bool
relacionesValidas = undefined

usuariosDeRelacionValidos :: [Usuario] -> [Relacion] -> Bool
usuariosDeRelacionValidos = undefined

relacionesAsimetricas :: [Relacion] -> Bool
relacionesAsimetricas = undefined

-- Aaron
noHayRelacionesRepetidas :: [Relacion] -> Bool
noHayRelacionesRepetidas = undefined

publicacionesValidas :: [Usuario] -> [Publicacion] -> Bool
publicacionesValidas = undefined

usuariosDePublicacionSonUsuariosDeRed :: [Usuario] -> [Publicacion] -> Bool
usuariosDePublicacionSonUsuariosDeRed = undefined

noHayPublicacionesRepetidas :: [Publicacion] -> Bool
noHayPublicacionesRepetidas = undefined

cadenaDeAmigos :: [Usuario] -> RedSocial -> Bool
cadenaDeAmigos = undefined

-- Agustin
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
