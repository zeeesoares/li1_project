{- |
Module      : Tarefa2_2022li1g035
Description : Geração contínua de um mapa
Copyright   : José António Costa Soares <a103995@alunos.uminho.pt>
              Nuno Miguel Parente Morais <a104090@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g035 where

import LI12223
import Tarefa1_2022li1g035
import System.Random


{- | =Aleatoriedade
*As funções que se seguem são as responsáveis por atribuir uma aleatoriedade à função estendeMapa:

@
geraListaAleatorios  :: Semente -> Comprimento -> [Int]
geraListaAleatorios s c= take c $ randoms (mkStdGen s)
@

@
nAleatorio :: Semente -> Comprimento -> Int
nAleatorio s c = head(geraListaAleatorios s c)
@

@
geradorVelocidades :: Int ->  Int
geradorVelocidades random = (mod random 5)-2
@

== Exemplos de utilização:
>>> estendeMapa (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) 12
Mapa 3 [(Rio 0,[Tronco,Nenhum,Tronco]),(Relva,[Arvore,Arvore,Nenhum])]
-}

type Comprimento = Int
type Semente = Int

geraListaAleatorios  :: Semente -> Comprimento -> [Int]
geraListaAleatorios s c= take c $ randoms (mkStdGen s)

nAleatorio :: Semente -> Comprimento -> Int
nAleatorio s c = head(geraListaAleatorios s c)


geradorVelocidades :: Int ->  Int
geradorVelocidades random = (mod random 4)-2

{- | =estendeMapa
*A funçõo estendeMapa deve gerar e adicionar uma nova linha valida ao topo de um dado mapa.
Alternativamente, a função poderia ser definida da seguinte forma:

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l linhas) x = Mapa l ((juntaTudo x (Mapa l linhas) ((proximoTerreno x (Mapa l linhas)))): linhas)
@


== Exemplos de utilização:
>>> estendeMapa (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) 12
Mapa 3 [(Rio 0,[Tronco,Nenhum,Tronco]),(Relva,[Arvore,Arvore,Nenhum])]
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l linhas) x 
    | mapaValido (Mapa l ((juntaTudo x (Mapa l linhas) ((proximoTerreno x (Mapa l linhas)))): linhas)) = Mapa l ((juntaTudo x (Mapa l linhas) ((proximoTerreno x (Mapa l linhas)))): linhas)
    | otherwise = estendeMapa (Mapa l linhas) (x+1)

{- =Próximo Terreno
*|A função proximoTerreno deve gerar a lista de terrenos passíveis de serem usados numa nova linha no topo do mapa dado.
Alternativamente, a função poderia ser definida da seguinte forma:

@
proximoTerreno :: Int -> Mapa -> (Terreno,[Obstaculo])
proximoTerreno x (Mapa l linhas) = (proximosTerrenosValidos (Mapa l linhas) !! mod x (length (proximosTerrenosValidos (Mapa l linhas) )),[])

justTerreno :: (Terreno,[Obstaculo]) -> Terreno
justTerreno  (t, obs) = t

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]
proximosTerrenosValidos (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada 0,Relva]
proximosTerrenosValidos (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio 0,Relva]
proximosTerrenosValidos (Mapa _ ((Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):t)) = [Estrada 0, Rio 0]
proximosTerrenosValidos (Mapa _ lista) = [Rio 0, Estrada 0, Relva]
@

== Exemplos de utilização:
>>> proximoTerreno 12 (Mapa 10 [])
(Rio 0,[])

-}
-- Escolhe numero de [0..100]
proximoTerreno :: Int -> Mapa -> (Terreno,[Obstaculo])
proximoTerreno x (Mapa l linhas) = (proximosTerrenosValidos x (Mapa l linhas) !! mod x (length (proximosTerrenosValidos x (Mapa l linhas) )),[])

justTerreno :: (Terreno,[Obstaculo]) -> Terreno
justTerreno  (t, obs) = t

proximosTerrenosValidos :: Int -> Mapa -> [Terreno]
proximosTerrenosValidos x (Mapa _ []) = [Rio (geradorVelocidades x), Estrada (geradorVelocidades x), Relva]
proximosTerrenosValidos x (Mapa _ ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t)) = [Estrada (geradorVelocidades x),Relva]
proximosTerrenosValidos x (Mapa _ ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = [Rio (geradorVelocidades x),Relva]
proximosTerrenosValidos x (Mapa _ ((Relva,_):(Relva,_):(Relva,_):(Relva,_):(Relva,_):t)) = [Estrada (geradorVelocidades x), Rio (geradorVelocidades x)]
proximosTerrenosValidos x (Mapa _ lista) = [Rio (geradorVelocidades x), Estrada (geradorVelocidades x), Relva]




{- =Próximos Obstáculos
*|A função próximosObs deve gerar a lista de obstáculos passiveis de serem usados para continuar uma dada linha do mapa.
Alternativamente, a função poderia ser definida da seguinte forma:

@
proximoObs :: Int -> Mapa -> (Terreno,[Obstaculo]) -> [Obstaculo]
proximoObs x (Mapa 0 linhas) (t, obs) = []
proximoObs x (Mapa l linhas) (t, obs) = proximosObstaculosValidos x (t,obs) !! mod x (length (proximosObstaculosValidos x (t,obs))): proximoObs (x+1) (Mapa (l-1) linhas) (t, proximosObstaculosValidos x (t,obs) !! mod x (length (proximosObstaculosValidos x (t,obs))):obs)
@

== Exemplos de utilização:
>>> proximoObs 12 (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) (Rio 0,[])
[Tronco,Nenhum,Tronco]
-}

{- |A função juntaTudo deve juntar tanto o terreno gerado como a lista de obstáculos que tambem foi gerada e assim formar uma nova linha.
Alternativamente, a função poderia ser definida da seguinte forma:

@
juntaTudo ::  Int -> Mapa -> (Terreno,[Obstaculo]) -> (Terreno,[Obstaculo])
juntaTudo x (Mapa l linhas) (t, obs) = (t,proximoObs x (Mapa l linhas) (t, obs))
@

== Exemplos de utilização:
>>> juntaTudo 12 (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) (Rio 0,[])
(Rio 0,[Tronco,Nenhum,Tronco])

-}
--Junta terrenos e os seus obstaculos.
juntaTudo ::  Int -> Mapa -> (Terreno,[Obstaculo]) -> (Terreno,[Obstaculo])
juntaTudo x (Mapa l linhas) (t, obs) = (t,proximoObs x (Mapa l linhas) (t, obs))

--seleciona a lista de obstaculos
proximoObs :: Int -> Mapa -> (Terreno,[Obstaculo]) -> [Obstaculo]
proximoObs x (Mapa 0 linhas) (t, obs) = []
proximoObs x (Mapa l linhas) (t, obs) = proximosObstaculosValidos x (t,obs) !! mod (head(geraListaAleatorios x 1)) (length (proximosObstaculosValidos x (t,obs))): proximoObs (x+1) (Mapa (l-1) linhas) (t, proximosObstaculosValidos x (t,obs) !! mod (head(geraListaAleatorios x 1)) (length (proximosObstaculosValidos x (t,obs))):obs)

-- Obstaculos possiveis.
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos n (t, obs)
    | n <= length obs = []
proximosObstaculosValidos n (Rio _, (Tronco:Tronco:Tronco:Tronco:Tronco:t)) = [Nenhum]
proximosObstaculosValidos n (Estrada _, (Carro:Carro:Carro:t)) = [Nenhum]
proximosObstaculosValidos n (Relva, (Arvore:Arvore:t)) = [Nenhum]
proximosObstaculosValidos n (t, obs)
    | ((not (elem Nenhum obs)) && (n == ((-1) + (length obs)))) = [Nenhum]
    | otherwise = proximosObsAUX n (t, obs)

proximosObsAUX :: Int -> (Terreno,[Obstaculo]) -> [Obstaculo]
proximosObsAUX n (Rio _, obs) = [Tronco,Nenhum]
proximosObsAUX n (Estrada _, obs) = [Nenhum,Carro]
proximosObsAUX n (Relva, obs) = [Nenhum,Arvore]
