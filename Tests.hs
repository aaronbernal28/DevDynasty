import Test.HUnit
import iap1-tp

main = runTestTT todosLosTest
todosLosTest = test [testsuite1,testsuite2,testsuite3,testsuite4,testsuite5,testsuite6,testsuite7,testsuite8,testsuite9,testsuite10]

-- testsuite ejercicio 1

testsuite1 = test [
    "Caso 1A: red con 1 usuario"        ~: (nombresDeUsuarios red1A) ~?= ["lucas"],
    "Caso 1B: red con mas de 1 usuario" ~: (nombresDeUsuarios red1B) ~?= ["lucas","mateo","santiago","valentina","renata","agustin","julieta","Roberto Carlos"]
    ]

red1A :: RedSocial
red1A = ([lucas], [], [])
red1B :: RedSocial
red1B = (usuarios0, [], []) 

-- testsuite ejercicio 2

testsuite2 = test [
    "Caso 2A: red no tiene relaciones"                         ~: (amigosDe red2A lucas) ~?= [],
    "Caso 2B: red tiene 1 relacion pero no de u"               ~: (amigosDe red2B mateo) ~?= [],
    "Caso 2C: red tiene 1 relacion y es de u"                  ~: (amigosDe red2C santiago) ~?= [lucas],
    "Caso 2D: red tiene mas de 1 relacion y 1 es de u"         ~: (amigosDe red2D valentina) ~?= [renata],
    "Caso 2E: red tienen mas de 1 relacion y mas de 1 es de u" ~: (amigosDe red2E renata) ~?= [santiago,valentina,lucas]
    ]

red2A :: RedSocial
red2A = (usuarios0, [], [])
red2B :: RedSocial
red2B = (usuarios0, [(lucas,santiago)], []) 
red2C :: RedSocial
red2C = (usuarios0, [(lucas,santiago)], [])
red2D :: RedSocial
red2D = (usuarios0, relaciones2, [])
red2E :: RedSocial
red2E = (usuarios0, relaciones2, [])
relaciones2 :: [Relacion]
relaciones2 = [(lucas,mateo), (santiago,renata), (valentina,renata), (lucas,renata)]

-- testsuite ejercicio 3

testsuite3 = test [
    "Caso 3A: red no tiene relaciones"                         ~: (cantidadDeAmigos red2A lucas) ~?= 0,
    "Caso 3B: red tiene 1 relacion pero no de u"               ~: (cantidadDeAmigos red2B mateo) ~?= 0,
    "Caso 3C: red tiene 1 relacion y es de u"                  ~: (cantidadDeAmigos red2C santiago) ~?= 1,
    "Caso 3D: red tiene mas de 1 relacion y 1 es de u"         ~: (cantidadDeAmigos red2D valentina) ~?= 1,
    "Caso 3E: red tienen mas de 1 relacion y mas de 1 es de u" ~: (cantidadDeAmigos red2E renata) ~?= 3
    ]

-- testsuite ejercicio 4

testsuite4 = test [
    "Caso 4A: 1 usuario tiene 1 amigo"                ~: (usuarioConMasAmigos red4A) ~?= lucas,
    "Caso 4B: 1 usuario tiene mas de un amigo"        ~: (usuarioConMasAmigos red4B) ~?= santiago,
    "Caso 4C: mas de 1 usuario tiene mas de un amigo" ~: (usuarioConMasAmigos red4C) ~?= valentina
    ]

red4A :: RedSocial
red4A = (usuarios0, relaciones4A, [])
relaciones4A :: [Relacion]
relaciones4A = [(lucas,mateo)]

red4B :: RedSocial
red4B = (usuarios0, relaciones4B, [])
relaciones4B :: [Relacion]
relaciones4B = [(mateo,santiago), (santiago,valentina)]

red4C :: RedSocial
red4C = (usuarios0, relaciones4C, [])
relaciones4C :: [Relacion]
relaciones4C = [(lucas,mateo), (santiago,valentina), (lucas,renata),(valentina,renata),(valentina,mateo)]

-- testsuite ejercicio 5

testsuite5 = test [
    "Caso 5A: red no tiene un Roberto Carlos" ~: (estaRobertoCarlos red5A) ~?= False,
    "Caso 5B: red tiene un Roberto Carlos"    ~: (estaRobertoCarlos red5B) ~?= True
    ]

red5A :: RedSocial
red5A = ([lucas,mateo], [(lucas,mateo)], [])
red5B :: RedSocial
red5B = (usuarios0, relaciones5B, []) --solo pongo usuarios ya que sino tardaria demasiado en verificar el millon de usuarios

usuarios5B :: [Usuario]
usuarios5B = roberto : usuarios5BAux -- lista real de usuarios
usuarios5BAux :: [Usuario]
usuarios5BAux = listaDeNUsuariosHasta 1 1000001

listaDeNUsuariosHasta :: Integer -> Integer -> [Usuario]
listaDeNUsuariosHasta n m
    | n > m = []
    | otherwise = (n,"name") : listaDeNUsuariosHasta (n+1) m

relaciones5B :: [Relacion]
relaciones5B = relacionarConRoberto roberto usuarios5BAux

relacionarConRoberto :: Usuario -> [Usuario] -> [Relacion]
relacionarConRoberto r [] = []
relacionarConRoberto r (u:us) = (r,u) : relacionarConRoberto r us

-- testsuite ejercicio 6

testsuite6 = test [
    "Caso 6A: ninguna publicacion es de u"  ~: (publicacionesDe red6 renata) ~?= [],
    "Caso 6B: 1 publicacion es de u"        ~: (publicacionesDe red6 lucas) ~?= [(lucas,pub1,[])],
    "Caso 6C: mas de 1 publicacion es de u" ~: (publicacionesDe red6 valentina) ~?= [(valentina,pub4,[]), (valentina,pub5,[]), (valentina,pub6,[])]
    ]

red6 :: RedSocial
red6 = (usuarios0, [], publicaciones6)
publicaciones6 :: [Publicacion]
publicaciones6 = [(lucas,pub1,[]), (mateo,pub2,[]), (mateo,pub3,[]), (valentina,pub4,[]), (valentina,pub5,[]), (valentina,pub6,[])]

-- testsuite ejercicio 7

testsuite7 = test [
    "Caso 7B: a u no le gusta ninguna publicacion" ~: (publicacionesQueLeGustanA red7 valentina) ~?= [],
    "Caso 7C: a u le gusta 1 publicacion"          ~: (publicacionesQueLeGustanA red7 renata) ~?= [(lucas,pub1,[renata,mateo])],
    "Caso 7D: a u le gusta mas de 1 publicacion"   ~: (publicacionesQueLeGustanA red7 mateo) ~?= [(lucas,pub1,[renata,mateo]), (valentina,pub4,[mateo]), (valentina,pub5,[mateo])]
    ]

red7 :: RedSocial
red7 = (usuarios0, [], publicaciones7)
publicaciones7 :: [Publicacion]
publicaciones7 = [(lucas,pub1,[renata,mateo]), (mateo,pub2,[]), (mateo,pub3,[]), (valentina,pub4,[mateo]), (valentina,pub5,[mateo]), (valentina,pub6,[])]

-- testsuite ejercicio 8

testsuite8 = test [
    "Caso 8A: a u1 y u2 les gusta las mismas publacaciones"            ~: (lesGustanLasMismasPublicaciones red8A mateo lucas) ~?= True,
    "Caso 8B: algunas publicaciones les gusta a u1 y u2 pero no todas" ~: (lesGustanLasMismasPublicaciones red8B santiago mateo) ~?= False,
    "Caso 8C: a u1 y u2 les gusta distintas publicaciones"             ~: (lesGustanLasMismasPublicaciones red8C julieta valentina) ~?= False
    ]

red8A :: RedSocial
red8A = (usuarios0, [], publicaciones8)
red8B :: RedSocial
red8B = (usuarios0, [], publicaciones8)
red8C :: RedSocial
red8C = (usuarios0, [], publicaciones8)
publicaciones8 :: [Publicacion]
publicaciones8 = [(lucas,pub1,[julieta]), (mateo,pub2,[julieta]), (santiago,pub3,[mateo,lucas,julieta]), (valentina,pub4,[]), (renata,pub5,[santiago,valentina]), (agustin,pub6,[mateo,lucas,santiago,valentina]), (julieta,pub7,[valentina])]

-- testsuite ejercicio 9

testsuite9 = test [
    "Caso 9A: u no tiene publicaciones"                                            ~: (tieneUnSeguidorFiel red9A lucas) ~?= False,
    "Caso 9B: u tiene mas de 1 publicacion pero a nadie le gusta todas"            ~: (tieneUnSeguidorFiel red9B mateo) ~?= False,
    "Caso 9C: u tiene mas de 1 publicacion y existe un usuario que le gusto todas" ~: (tieneUnSeguidorFiel red9C valentina) ~?= True
    ]

red9A :: RedSocial
red9A = (usuarios0, [], [])
red9B :: RedSocial
red9B = (usuarios0, [], publicaciones9)
red9C :: RedSocial
red9C = (usuarios0, [], publicaciones9)
publicaciones9 :: [Publicacion]
publicaciones9 = [(mateo,pub1,[lucas,santiago]), (mateo,pub2,[santiago,renata]), (mateo,pub3,[]), (valentina,pub4,[lucas,santiago]), (valentina,pub5,[santiago,renata]), (valentina,pub6,[santiago,agustin]), (valentina,pub7,[santiago])]

-- testsuite ejercicio 10

testsuite10 = test [
    "Caso 10A: u1 y u2 tienen mas de 1 rel pero no hay secuencia"     ~: (existeSecuenciaDeAmigos red10A lucas roberto) ~?= False,
    "Caso 10B: u1 y u2 estan relacionados"                            ~: (existeSecuenciaDeAmigos red10B lucas roberto) ~?= True,
    "Caso 10C: u1 y u2 tienen un amigo en comun"                      ~: (existeSecuenciaDeAmigos red10C lucas roberto) ~?= True,
    "Caso 10D: existe una cadena de relaciones que vincula a u1 y u2" ~: (existeSecuenciaDeAmigos red10D lucas roberto) ~?= True,
    "Caso 10E: u1 y u2 son el mismo usuario y u1 tiene al menos una relación de amistad" ~: (existeSecuenciaDeAmigos red10E lucas lucas) ~?= True,
    "Caso 10F: u1 y u2 son el mismo usuario y u1 no tiene relaciones de amistad" ~: (existeSecuenciaDeAmigos red10F lucas lucas) ~?= False
    ]

red10A :: RedSocial
red10A = (usuarios0, relaciones10A, [])
relaciones10A :: [Relacion]
relaciones10A = [(lucas,mateo), (julieta,roberto), (mateo,santiago)]
red10B :: RedSocial
red10B = (usuarios0, relaciones10B, [])
relaciones10B :: [Relacion]
relaciones10B = [(lucas,roberto)]
red10C :: RedSocial
red10C = (usuarios0, relaciones10C, [])
relaciones10C :: [Relacion]
relaciones10C = [(lucas,valentina),(roberto,valentina)]
red10D :: RedSocial
red10D = (usuarios0, relaciones10D, [])
relaciones10D :: [Relacion]
relaciones10D = [(lucas,mateo), (mateo,santiago), (santiago,valentina), (valentina,renata), (renata,agustin), (agustin,julieta), (julieta,roberto)]
red10E :: RedSocial
red10E = (usuarios0, relaciones10E, [])
relaciones10E :: [Relacion]
relaciones10E = [(lucas,mateo), (mateo,santiago), (santiago,valentina), (valentina,renata), (renata,agustin), (agustin,julieta), (julieta,roberto)]
red10F :: RedSocial
red10F = (usuarios0, relaciones10F, [])
relaciones10F :: [Relacion]
relaciones10F = [(mateo,santiago), (santiago,valentina), (valentina,renata), (renata,agustin), (agustin,julieta), (julieta,roberto)]

-- usuarios y publicaciones de ejermplo

usuarios0 :: [Usuario]
usuarios0 = [lucas,mateo,santiago,valentina,renata,agustin,julieta,roberto]

lucas :: Usuario
lucas = (1, "lucas")
mateo :: Usuario
mateo = (2, "mateo")
santiago :: Usuario
santiago = (3, "santiago")
valentina :: Usuario
valentina = (4, "valentina")
renata :: Usuario
renata = (5, "renata")
agustin :: Usuario
agustin = (6, "agustin")
julieta :: Usuario
julieta = (7, "julieta")
roberto :: Usuario
roberto = (8,"Roberto Carlos")

pub1 :: String
pub1 = "Just wrote a new blog post on my website!"
pub2 :: String
pub2 = "I'm so excited to announce my new project!"
pub3 :: String
pub3 = "Just finished a long day of coding, time to relax!"
pub4 :: String
pub4 = "Can't wait to attend the upcoming tech conference!"
pub5 :: String
pub5 = "I'm proud to be a software engineer!"
pub6 :: String
pub6 = "Just learned a new coding language, feeling accomplished!"
pub7 :: String
pub7 = "Coding can be frustrating at times, but it's so rewarding when you finally figure it out!"


