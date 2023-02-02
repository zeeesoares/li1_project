{- |
Module      : Tarefa1_2022li1g035
Description : Validação de um mapa
Copyright   : José António Costa Soares <a103995@alunos.uminho.pt>
              Nuno Miguel Parente Morais <a104090@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g035 where

import LI12223

{- | =Mapa Válido
*A funçõo mapaValido é a função principal que nos valida um mapa.

== Corpo da função:

@
mapaValido :: Mapa -> Bool
mapaValido m@(Mapa l (h@(terreno,obs):t)) = 
    terrenoMapa m && troncoMapa m && contiguosMapa m && carroMapa m && linhaValida m && larguraValida m && terrenosSucessivos m
@

-}
mapaValido :: Mapa -> Bool
mapaValido m@(Mapa l (h@(terreno,obs):t)) = terrenoMapa m && troncoMapa m && contiguosMapa m && carroMapa m && linhaValida m && larguraValida m && terrenosSucessivos m


--1 OBSTACULOS NO TERRENO CORRETO
{- | =OBSTACULOS NO TERRENO CORRETO 
*A funçõo terrenoMapa valida se em determindado mapa e em determinado terreno apenas estão colocados os obstaculos adequados.

== Corpo da função:

@
terrenoMapa :: Mapa -> Bool
terrenoMapa (Mapa l []) = False
terrenoMapa (Mapa l (h:[])) = terrenoValido h
terrenoMapa (Mapa l (h@(terreno,obs):t)) = terrenoValido h && terrenoMapa (Mapa l t)
@

== Exemplos de utilização:
>>> terrenoMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])])
True


== Funções Auxiliares

@
terrenoValido :: (Terreno,[Obstaculo]) -> Bool
terrenoValido (_,[]) = False
terrenoValido (Rio _,obs)
    | elem Arvore obs || elem Carro obs = False
    | otherwise = True
terrenoValido (Relva,obs)
    | elem Tronco obs || elem Carro obs = False
    | otherwise = True
terrenoValido (Estrada _,obs)
    | elem Tronco obs || elem Arvore obs = False
    | otherwise = True
@


== Exemplos de utilização:
>>> terrenoValido (Rio (-1),[Tronco,Tronco,Nenhum,Tronco])
True
-}

terrenoMapa :: Mapa -> Bool
terrenoMapa (Mapa l []) = False
terrenoMapa (Mapa l (h:[])) = terrenoValido h
terrenoMapa (Mapa l (h@(terreno,obs):t)) = terrenoValido h && terrenoMapa (Mapa l t)


terrenoValido :: (Terreno,[Obstaculo]) -> Bool
terrenoValido (_,[]) = False
terrenoValido (Rio _,obs)
    | elem Arvore obs || elem Carro obs = False
    | otherwise = True
terrenoValido (Relva,obs)
    | elem Tronco obs || elem Carro obs = False
    | otherwise = True
terrenoValido (Estrada _,obs)
    | elem Tronco obs || elem Arvore obs = False
    | otherwise = True





-- 2 RIOS CONTIGUOS

{- | =RIOS CONTIGUOS 
*A funçõo contiguosMapa valida se em determindado mapa, rios que são contiguos têm direções opostas.

== Corpo da função:

@
contiguosMapa :: Mapa -> Bool
contiguosMapa (Mapa l []) = False
contiguosMapa (Mapa l [(Rio x,obs)]) = True
contiguosMapa (Mapa l [(_,obs)]) = True
contiguosMapa (Mapa l ((Rio x,obs1):(Rio y,obs2):t)) 
    | x > 0 && y > 0 = False
    | x < 0 && y < 0 = False
contiguosMapa (Mapa l (h:t)) = contiguosMapa (Mapa l t)
@

== Exemplos de utilização:
>>> contiguosMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])])
True

-}


contiguosMapa :: Mapa -> Bool
contiguosMapa (Mapa l []) = False
contiguosMapa (Mapa l [(Rio x,obs)]) = True
contiguosMapa (Mapa l [(_,obs)]) = True
contiguosMapa (Mapa l ((Rio x,obs1):(Rio y,obs2):t)) 
    | x > 0 && y > 0 = False
    | x < 0 && y < 0 = False
contiguosMapa (Mapa l (h:t)) = contiguosMapa (Mapa l t)




-- 3 TRONCOS VALIDOS


{- | = TRONCOS VALIDOS
 *A funçõo troncoMapa valida se em determindado mapa e no caso dos terrenos serem Rios, não existem mais de 5 troncos seguidos.

== Corpo da função:

@
troncoMapa :: Mapa -> Bool
troncoMapa (Mapa l []) = False
troncoMapa (Mapa l [(Rio _,obs)]) = troncosValidos obs
troncoMapa (Mapa l ((terreno,obs):t)) = troncosValidos (myhead (selectRios ((Mapa l ((terreno,obs):t))))) || troncoMapa (Mapa l t)
@

== Exemplos de utilização:
>>> troncoMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])])
True


== Funções Auxiliares

* Seleciona todos os rios dentro de um mapa.

@
selectRios :: Mapa -> [[Obstaculo]]
selectRios (Mapa l []) = []
selectRios (Mapa l [(terreno,obs)]) = if isRio terreno then [obs] else []
selectRios (Mapa l ((terreno,obs):t))
    | isRio terreno = obs: selectRios (Mapa l t)
    | otherwise = selectRios (Mapa l t)
@

* Fuinção que realmente valida a quantidade de troncos de um rio.

@
troncosValidos :: [Obstaculo] -> Bool
troncosValidos (h:[]) = True
troncosValidos [] = False
troncosValidos (h:t)
    | elem Tronco (myhead(contaObs (h:t))) == elem Tronco (last(contaObs (h:t))) = (length (myhead(contaObs (h:t))) + length (last(contaObs (h:t)))) <= 5 && troncosValidosAux (h:t)
    | otherwise = troncosValidosAux (h:t)

troncosValidosAux :: [Obstaculo] -> Bool
troncosValidosAux (h:[]) = True
troncosValidosAux [] = False
troncosValidosAux (h:t) = length (myhead (contaTroncos (h:t))) <= 5 && troncosValidosAux t 
@

* Fuinção que dentro de uma lista de obstáculos, analisa a existencia de troncos e agrupa-os respetivamente.

@
contaTroncos :: [Obstaculo] -> [[Obstaculo]]
contaTroncos [] = []
contaTroncos (h:t)
    | Tronco `elem` myhead (contaObs (h:t)) = myhead (contaObs (h:t)): contaTroncos (drop (length (myhead (contaObs (h:t)))) (h:t))
    | otherwise = contaTroncos (drop (length (myhead (contaObs (h:t)))) (h:t))
@

* Função que verifica se o terreno e um rio  

@
isRio :: Terreno -> Bool
isRio (Rio _) = True
isRio _ = False 
@

-}

troncoMapa :: Mapa -> Bool
troncoMapa (Mapa l []) = True
troncoMapa (Mapa l [(Rio _,obs)]) = troncosValidos obs
troncoMapa (Mapa l ((terreno,obs):t)) = troncosValidos (myhead (selectRios ((Mapa l ((terreno,obs):t))))) && troncoMapa (Mapa l t)


selectRios :: Mapa -> [[Obstaculo]]
selectRios (Mapa l []) = []
selectRios (Mapa l [(terreno,obs)]) = if isRio terreno then [obs] else []
selectRios (Mapa l ((terreno,obs):t))
    | isRio terreno = obs: selectRios (Mapa l t)
    | otherwise = selectRios (Mapa l t)

troncosValidos :: [Obstaculo] -> Bool
troncosValidos (h:[]) = True
troncosValidos [] = True
troncosValidos (h:t)
    | elem Tronco (myhead(contaObs (h:t))) && elem Tronco (last(contaObs (h:t))) = (length (myhead(contaObs (h:t))) + length (last(contaObs (h:t)))) <= 5 && troncosValidosAux (h:t)
    | otherwise = troncosValidosAux (h:t)

troncosValidosAux :: [Obstaculo] -> Bool
troncosValidosAux (h:[]) = True
troncosValidosAux [] = False
troncosValidosAux (h:t) = length (myhead (contaTroncos (h:t))) <= 5 && troncosValidosAux t 



contaTroncos :: [Obstaculo] -> [[Obstaculo]]
contaTroncos [] = []
contaTroncos (h:t)
    | Tronco `elem` myhead (contaObs (h:t)) = myhead (contaObs (h:t)): contaTroncos (drop (length (myhead (contaObs (h:t)))) (h:t))
    | otherwise = contaTroncos (drop (length (myhead (contaObs (h:t)))) (h:t))
 
isRio :: Terreno -> Bool
isRio (Rio _) = True
isRio _ = False 



--4
{- | =CARROS VÁLIDOS
*A funçõo carroMapa valida se em determindado mapa e no caso dos terrenos serem Estraadas, não existem mais de 5 troncos seguidos.

== Corpo da função:

@
carroMapa :: Mapa -> Bool
carroMapa (Mapa l []) = True
carroMapa (Mapa l [(Estrada _,obs)]) = carrosValidos obs
carroMapa (Mapa l ((terreno,obs):t)) = carrosValidos (myhead (selectEstradas (Mapa l ((terreno,obs):t)))) || carroMapa (Mapa l t)
@

== Exemplos de utilização:
>>> carroMapa (Mapa 4 [(Estrada (-1),[Carro,Carro,Nenhum,Carro]),(Estrada 1,[Carro,Carro,Nenhum,Nenhum])])
True


== Funções Auxiliares

* Seleciona todos as estradas dentro de um mapa.

@
selectEstradas :: Mapa -> [[Obstaculo]]
selectEstradas (Mapa l []) = []
selectEstradas (Mapa l [(terreno,obs)]) = if isEstrada terreno then [obs] else []
selectEstradas (Mapa l ((terreno,obs):t))
    | isEstrada terreno = obs: selectEstradas (Mapa l t)
    | otherwise = selectEstradas (Mapa l t)
@

* Fuinção que realmente valida a quantidade de carros de uma estrada.

@
carrosValidos :: [Obstaculo] -> Bool
carrosValidos (h:[]) = True
carrosValidos [] = False
carrosValidos (h:t)
    | elem Carro (myhead(contaObs (h:t))) == elem Carro (last(contaObs (h:t))) = (length (myhead(contaObs (h:t))) + length (last(contaObs (h:t)))) <= 3 && carrosValidosAux (h:t)
    | otherwise = carrosValidosAux (h:t)

carrosValidosAux :: [Obstaculo] -> Bool
carrosValidosAux [] = False
carrosValidosAux (h:[]) = True
carrosValidosAux (h:t) = length (myhead (contaCarros (h:t))) <= 3 && carrosValidosAux t 
@

* Fuinção que dentro de uma lista de obstáculos, analisa a existencia de troncos e agrupa-os respetivamente.

@
contaCarros :: [Obstaculo] -> [[Obstaculo]]
contaCarros [] = []
contaCarros (h:t)
    | Carro `elem` myhead (contaObs (h:t)) = myhead (contaObs (h:t)): contaCarros (drop (length (myhead (contaObs (h:t)))) (h:t))
    | otherwise = contaCarros (drop (length (myhead (contaObs (h:t)))) (h:t))
@

* Função que verifica se o terreno e uma estrada.  

@
isEstrada :: Terreno -> Bool
isEstrada (Estrada _) = True
isEstrada _ = False 
@
-}


carroMapa :: Mapa -> Bool
carroMapa (Mapa l []) = True
carroMapa (Mapa l [(Estrada _,obs)]) = carrosValidos obs
carroMapa (Mapa l ((terreno,obs):t)) = carrosValidos (myhead (selectEstradas (Mapa l ((terreno,obs):t)))) && carroMapa (Mapa l t)

myhead :: [[Obstaculo]] -> [Obstaculo]
myhead [] = []
myhead [[h]] = [h]
myhead (h:t) = h

selectEstradas :: Mapa -> [[Obstaculo]]
selectEstradas (Mapa l []) = []
selectEstradas (Mapa l [(terreno,obs)]) = if isEstrada terreno then [obs] else []
selectEstradas (Mapa l ((terreno,obs):t))
    | isEstrada terreno = obs: selectEstradas (Mapa l t)
    | otherwise = selectEstradas (Mapa l t)

carrosValidos :: [Obstaculo] -> Bool
carrosValidos [] = True
carrosValidos [h] = length (myhead(contaCarros [h])) <= 3
carrosValidos (h:t)
    | elem Carro (myhead(contaObs (h:t))) && elem Carro (last(contaObs (h:t))) = (length (myhead(contaObs (h:t))) + length (last(contaObs (h:t)))) <= 3 && carrosValidosAux (h:t)
    | otherwise = carrosValidosAux (h:t)

carrosValidosAux :: [Obstaculo] -> Bool
carrosValidosAux [] = True
carrosValidosAux (h:[]) = True
carrosValidosAux (h:t) = length (myhead (contaCarros (h:t))) <= 3 && carrosValidosAux t 

-- Seleciona Carros da lista de obstaculos

contaCarros :: [Obstaculo] -> [[Obstaculo]]
contaCarros [] = []
contaCarros (h:t)
    | Carro `elem` myhead (contaObs (h:t)) = myhead (contaObs (h:t)): contaCarros (drop (length (myhead (contaObs (h:t)))) (h:t))
    | otherwise = contaCarros (drop (length (myhead (contaObs (h:t)))) (h:t))

--Verifica se o terreno e uma estrada 
isEstrada :: Terreno -> Bool
isEstrada (Estrada _) = True
isEstrada _ = False 

{- | =GROUP OBSTÁCULOS
*A funçõo contaObs nesta tarefa funciona como uma versão da função GROUP e é responsavel por organizar os obstaculos em blocos.

== Corpo da função:

@
contaObs :: Eq a => [a] -> [[a]]
contaObs [] = []
contaObs [x] = [[x]]
contaObs l = u: contaObs (drop (length u) l)
    where u = contaObsaux l
    
contaObsaux :: Eq a => [a] -> [a]
contaObsaux [] = []
contaObsaux [x] = [x]
contaObsaux (h:h1:t)
    | h == h1 = h: contaObsaux (h1:t) 
    | otherwise = [h]
@

== Exemplos de utilização:
>>> contaObs [Tronco,Tronco,Nenhum,Tronco]
[[Tronco,Tronco],[Nenhum],[Tronco]]

-}

-- Organiza lista de Obstaculos GROUP
contaObs :: Eq a => [a] -> [[a]]
contaObs [] = []
contaObs [x] = [[x]]
contaObs l = u: contaObs (drop (length u) l)
    where u = contaObsaux l
    
contaObsaux :: Eq a => [a] -> [a]
contaObsaux [] = []
contaObsaux [x] = [x]
contaObsaux (h:h1:t)
    | h == h1 = h: contaObsaux (h1:t) 
    | otherwise = [h]


--5 LINHA COM PASSAGEM

{- | =LINHA COM PASSAGEM
*A função linhaValida valida se num determinado terreno existe uma passagem (Nenhum).

== Corpo da função:

@
linhaValida :: Mapa -> Bool
linhaValida (Mapa l []) = False
linhaValida (Mapa l [(terreno,obs)]) = Nenhum `elem` obs
linhaValida (Mapa l ((terreno,obs):t)) =  Nenhum `elem` obs  &&  linhaValida (Mapa l t)
@

== Exemplos de utilização:
>>> linhaValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])])
True

-}

linhaValida :: Mapa -> Bool
linhaValida (Mapa l []) = False
linhaValida (Mapa l [(terreno,obs)]) = Nenhum `elem` obs
linhaValida (Mapa l ((terreno,obs):t)) =  Nenhum `elem` obs  &&  linhaValida (Mapa l t)

--6 O comprimento da lista de obstaculos de cada linha corresponde exactamente a largura do mapa
{- | =COMPRIMENTO == LARGURA
*A função larguraValida valida se num determinado o comprimento da lista de obstaculos corresponde à largura do mapa.

== Corpo da função:

@
larguraValida :: Mapa -> Bool
larguraValida (Mapa l []) = False
larguraValida (Mapa l [(t, obs)]) = length obs == l
larguraValida (Mapa l ((t, obs):r))
    | length obs == l = larguraValida (Mapa l r)
    | otherwise = False
@

== Exemplos de utilização:
>>> larguraValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])])
True

-}


larguraValida :: Mapa -> Bool
larguraValida (Mapa l []) = False
larguraValida (Mapa l [(t, obs)]) = length obs == l
larguraValida (Mapa l ((t, obs):r))
    | length obs == l = larguraValida (Mapa l r)
    | otherwise = False

--7 Contiguamente, nao devem existir mais do que 4 rios, nem 5 estradas ou relvas.
{- | =TERRENOS CONTIGUOS
*A função terrenosSucessivos valida se num determinado mapa não existem mais de 4 rios, 5 estradas e 5 relvas.

== Corpo da função:

@
larguraValida :: Mapa -> Bool
larguraValida (Mapa l []) = False
larguraValida (Mapa l [(t, obs)]) = length obs == l
larguraValida (Mapa l ((t, obs):r))
    | length obs == l = larguraValida (Mapa l r)
    | otherwise = False
@

== Exemplos de utilização:
>>> larguraValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])])
True

-}

terrenosSucessivos :: Mapa -> Bool
terrenosSucessivos (Mapa l []) = False
terrenosSucessivos (Mapa l [h]) = True
terrenosSucessivos (Mapa l ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = False
terrenosSucessivos (Mapa l ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = False
terrenosSucessivos (Mapa l ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t)) = False
terrenosSucessivos (Mapa l (h:t)) = terrenosSucessivos (Mapa l t)


-- As funcoes definidas a seguir servem para contar quantos Rios/Estradas/Relvas que aparecem seguidas 
-- no mapa recebido pela funcao MapaContiguoCheck

