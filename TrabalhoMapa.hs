import Mapa

aracaju :: Cidade
aracaju = ("Aracaju", (0.3,0.6), ["Maceio"])


maceio :: Cidade
maceio = ("Maceio", (0.3,0.6), ["Aracaju"])

-- // Função que retorna o primeiro item de uma tupla de 3
primeiro :: (a,b,c) -> a
primeiro (x,_ , _) = x

-- // Função que retorna o segundo item de uma tupla de 3
scn :: (a, b, c) -> b
scn (_ ,x , _) = x

-- // Função que retorna o terceiro item de uma tupla de 3
thd :: (a, b, c) -> c
thd (_, _, x) = x


-- // Função que retorna um mapa
getMap :: Mapa
getMap = [aracaju, maceio]

-- // Função que adiciona uma cidade a um mapa
addCity :: Mapa -> Nome -> Localizacao -> Mapa
addCity mapaUsed nameCity cordinates = (nameCity, cordinates, []): mapaUsed

-- // Função auxiliar que remove a cidade das Rotas que estão conectadas com a cidade removida e retorna as Rotas 
removeRouteCity :: Nome -> Rotas -> Rotas
removeRouteCity _ [] = []
removeRouteCity x (y:ys) 
 | x == y = removeRouteCity x ys
 | otherwise = y : removeRouteCity x ys

 -- // Função auxiliar que remove as Rotas da cidade removida e retorna o mapa sem a rota
removeRoute :: Nome -> Mapa -> Mapa
removeRoute _ [] = []
removeRoute x (z:zs) = cityWithoutRoute : removeRoute x zs
  where cityWithoutRoute = (primeiro(z), scn(z), removeRouteCity x (thd (z)))

-- // Função que remove uma cidade e suas Rotas do mapa e retorna um mapa sem a cidade e suas Rotas
removeCity :: Nome -> Mapa -> Mapa
removeCity _ [] = []
removeCity nameCity (x:xs)
 | nameCity == primeiro (x) = removeCity nameCity (removeRoute nameCity xs)
 | otherwise = x: removeCity nameCity xs

-- // Função que recebe dois nomes e um mapa e retorna um mapa com a rota adicionada
addRoute :: Nome -> Nome -> Mapa -> Mapa
addRoute _ _ [] = []
addRoute x y (z:zs)
 | x == primeiro (z) = cityWithRoute : addRoute y x zs
 | otherwise = z: addRoute x y zs
  where cityWithRoute = (x, scn(z), y: thd(z))



