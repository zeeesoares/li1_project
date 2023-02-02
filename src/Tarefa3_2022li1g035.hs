{- |
Module      : Tarefa3_2022li1g035
Description : Movimentação do personagem e obstáculos
Copyright   : José António Costa Soares <a103995@alunos.uminho.pt>
              Nuno Miguel Parente Morais <a104090@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g035 where
import Tarefa1_2022li1g035
import LI12223
{- | =animaJogo
*A funçõo animaJogo é a função principal que automatiza todos os elementos do mapa, sendo eles superfícies, osbtaculos ou o jogador.

== Corpo da função:

@
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((terreno,obs):t))) j = 
    juntaTudo (moveTerreno ((terreno, obs):t)) (jogadaValida (Jogador (x,y)) j (Mapa l ((terreno,obs):t))) l

@

-}

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo jogo@(Jogo (Jogador (x,y)) (Mapa l ((terreno,obs):t))) j 
    | ((j == Move Cima) && (((snd(((terreno,obs):t) !! (y-1)) !! x) == Carro))) = juntaTudo ((terreno, obs):t) (Jogador (x,y-1)) l
    | isEstrada (fst (((terreno,obs):t) !! (y))) && fst (verCarros (Jogador (x,y)) (((terreno,obs):t) !! (y))) == True = juntaTudo (moveTerreno (atropelado jogo) (Jogador (x,y)) j ) (jogadaValida (Jogador (x,y)) j (Mapa l ((terreno,obs):t))) l
    | otherwise = juntaTudo (moveTerreno ((terreno, obs):t) (Jogador (x,y)) j ) (jogadaValida (Jogador (x,y)) j (Mapa l ((terreno,obs):t))) l


atropelado :: Jogo -> [(Terreno, [Obstaculo])]
atropelado (Jogo (Jogador (x,y)) (Mapa l [])) = []
atropelado jogo@(Jogo (Jogador (x,y)) (Mapa l ((terreno,obs):t)))
    | isEstrada (terreno) && getVelocidade terreno > 0 && fst (verCarros (Jogador (x,y)) (terreno,obs)) == True && (x + (snd (verCarros (Jogador (x,y)) (terreno,obs)))) >= (x- (getVelocidade (terreno))) = (Estrada (snd(verCarros (Jogador (x,y)) (terreno,obs))),obs): atropelado (Jogo (Jogador (x,y)) (Mapa l t))
    | isEstrada (terreno) && getVelocidade terreno < 0 && fst (verCarros (Jogador (x,y)) (terreno,obs)) == True && (x + (snd (verCarros (Jogador (x,y)) (terreno,obs)))) <= (x- (getVelocidade (terreno))) = (Estrada ((-1)*snd(verCarros (Jogador (x,y)) (terreno,obs))),obs): atropelado (Jogo (Jogador (x,y)) (Mapa l t))
    | otherwise = (terreno,obs): atropelado (Jogo (Jogador (x,y)) (Mapa l t))

verCarros :: Jogador -> (Terreno, [Obstaculo]) -> (Bool,Int)
verCarros (Jogador (x,y)) (terreno,obs)
    | isEstrada terreno && ((obs !! (x-1)) == Carro || (obs !! (x+1)) == Carro) = (True,1)

-- | isEstrada (fst ( p !! (y))) && ((((snd(((terreno,obs):t) !! (y)) !! (x-1)) == Carro)) || (((snd(((terreno,obs):t) !! (y)) !! (x+1)) == Carro))) = (True,1)
    -- | isEstrada (fst ( p !! (y))) && ((((snd(((terreno,obs):t) !! (y)) !! (x-2)) == Carro))) = (True,2)
    | otherwise = (False,0)

-- 
{- | =juntaTudo
* A funcao juntaTudo recebe os argumentos das funcoes auxiliares moveTerreno e jogadaValida e devolve um argumento do tipo Jogo.

== Corpo da função:

@
juntaTudo :: [(Terreno, [Obstaculo])] -> Jogador -> Int -> Jogo 
juntaTudo ((terreno,obs):t) (Jogador (x,y)) l = (Jogo (Jogador (x,y)) (Mapa l ((terreno,obs):t)))
@

== Exemplos de utilização:
>>> juntaTudo [(Rio 1, [Nenhum, Tronco, Nenhum])] (Jogador (0,0)) 3
Jogo (Jogador (0,0)) (Mapa 3 [(Rio 1,[Nenhum,Tronco,Nenhum])]) -}

juntaTudo :: [(Terreno, [Obstaculo])] -> Jogador -> Int -> Jogo 
juntaTudo ((terreno,obs):t) (Jogador (x,y)) l = (Jogo (Jogador (x,y)) (Mapa l ((terreno,obs):t)))


--Funçao que faz os obstaculos moverem-se com a velocidade v dos terrenos (Rio ou Estrada)

{- | =moveTerreno
* A funcao moveTerreno que faz os obstaculos moverem-se com a velocidade v dos terrenos (Rio ou Estrada)

== Corpo da função:

@
moveTerreno :: [(Terreno, [Obstaculo])] -> [(Terreno, [Obstaculo])]
moveTerreno [] = []
moveTerreno ((terreno, obs):t)
    | isRio terreno = (terreno, transportador obs (getVelocidade terreno) 0) : moveTerreno t
    | isEstrada terreno = (terreno, transportador obs (getVelocidade terreno) 0) : moveTerreno t
@

== Exemplos de utilização:
>>> moveTerreno [(Rio 1, [Nenhum, Tronco, Nenhum])]
[(Rio 1,[Nenhum,Nenhum,Tronco])] -}

moveTerreno :: [(Terreno, [Obstaculo])] -> Jogador -> Jogada -> [(Terreno, [Obstaculo])]
moveTerreno [] _ _ = []
moveTerreno ((terreno, obs):t) (Jogador (x,y)) j 
    | isRio terreno = (terreno, transportador obs (getVelocidade terreno) 0) : moveTerreno t (Jogador (x,y)) j
    | isEstrada terreno = (terreno, transportadorEstrada ((terreno, obs):t) (getVelocidade terreno) 0 (Jogador (x,y)) j ) : (moveTerreno t (Jogador (x,y)) j)
    | isRelva terreno = (terreno, obs) : moveTerreno t (Jogador (x,y)) j

transportadorEstrada ::  [(Terreno, [Obstaculo])] -> Int -> Int -> Jogador -> Jogada ->  [Obstaculo]
transportadorEstrada ((terreno, (h:t)):c) v ac (Jogador (x,y)) j 
    | (( (v > ac) && ((h:t) !! (x-1) == Carro)) && (j == Parado)) = (last t : init (h:t))
    | (v == ac) = (h:t)
    | v > 0 =  transportador (last t : init (h:t)) v (ac+1) 
    | otherwise =  transportador (tail (h:t) ++ [h]) v (ac-1)


-- Retira o valor da velocidade de um determinado terreno
{- | =getVelocidade
* A funcao getVelocidade retira o valor da velocidade de um determinado terreno
== Corpo da função:

@
getVelocidade :: Terreno -> Int
getVelocidade (Rio v) = v
getVelocidade (Estrada v) = v
@

== Exemplos de utilização:
>>> getVelocidade (Rio 1)
1 -}

getVelocidade :: Terreno -> Int
getVelocidade (Rio v) = v
getVelocidade (Estrada v) = v
-- o input do acumulador deve ser 0
{- | =transportador
* A funcao transportador transporta os elementos de uma ponta da lista para outra conforme a velocidade do terreno.
== Corpo da função:

@
transportador :: [Obstaculo] -> Int -> Int -> [Obstaculo]
transportador (x:xs) v ac 
    | v == ac = (x:xs)
    | v > 0 = transportador (last xs : init (x:xs)) v (ac+1)
    | otherwise = transportador (tail (x:xs) ++ [x]) v (ac-1)
@

== Exemplos de utilização:
>>> transportador [Tronco, Tronco, Nenhum] 1 0
[Nenhum,Tronco,Tronco] -}

transportador :: [Obstaculo] -> Int -> Int -> [Obstaculo]
transportador (x:xs) v ac 
    | v == ac = (x:xs)
    | v > 0 = transportador (last xs : init (x:xs)) v (ac+1)
    | otherwise = transportador (tail (x:xs) ++ [x]) v (ac-1)

-- A Funçao moveTerrenoAux forma uma lista com listas de obstaculos atraves de um mapa.
moveTerrenoAux :: Mapa -> [[Obstaculo]]
moveTerrenoAux  (Mapa l []) = []
moveTerrenoAux  (Mapa l ((terreno,obs):t)) = obs : moveTerrenoAux (Mapa l t)



-- Funcao que verifica se a jogada pode ser realizada e realiza-a. 
{- | =jogadaValida
* A funcao jogadaValida verifica se a jogada pode ser realizada e realiza-a.

@
jogadaValida :: Jogador -> Jogada -> Mapa -> Jogador
jogadaValida (Jogador (x,y)) j (Mapa l ((terreno,obs):t))
    | myFst (moveJogador (Jogador (x,y)) j (Mapa l ((terreno,obs):t))) > l = Jogador (x,y)
    | (mySnd (moveJogador (Jogador (x,y)) j (Mapa l ((terreno,obs):t))) < 0 || (mySnd (moveJogador (Jogador (x,y)) j (Mapa l ((terreno,obs):t)))) > (length ((terreno,obs):t))) == True = Jogador (x,y)
    | otherwise = moveJogador (Jogador (x,y)) j (Mapa l ((terreno,obs):t))

@

== Exemplos de utilização:
>>> jogadaValida (Jogador (2,1)) (Move Cima) (Mapa 3 [(Rio 1,[Nenhum,Nenhum,Tronco]), (Relva, [Arvore, Nenhum, Nenhum])])
Jogador (2,0) 
-}

jogadaValida :: Jogador -> Jogada -> Mapa -> Jogador
jogadaValida (Jogador (x,y)) j (Mapa l p)
    | (j /= Parado ) && (myFst (moveJogador (Jogador (x,y)) j (Mapa l p))) > l-1 || (myFst (moveJogador (Jogador (x,y)) j (Mapa l p))) < 0  = Jogador (x,y)
    | (j == Parado) && ((snd ( p !! y ) !! (x) ) == Tronco) && (fst (x,y) == l-1)  = Jogador (x + getVelocidade ( fst (p !! y ) ), y)
    | (j == Parado) && ((snd ( p !! y ) !! (x) ) == Tronco) = Jogador (x + getVelocidade ( fst (p !! y ) ), y)
    | ((mySnd (moveJogador (Jogador (x,y)) j (Mapa l p))) == -1) && (j == (Move Cima)) || ((myFst (moveJogador (Jogador (x,y)) j (Mapa l p))) == -1) && (j == (Move Esquerda)) || ((myFst (moveJogador (Jogador (x,y)) j (Mapa l p))) == l) && (j == (Move Direita)) || (mySnd (moveJogador (Jogador (x,y)) j (Mapa l p)) == (length p)) && (j == (Move Baixo))  = Jogador (x,y)
    | (j == Move Cima) && (( snd ( p !! (y-1)) !! (x) ) == Arvore ) = Jogador (x,y)
    | (j == Move Baixo) && (( snd ( p !! (y+1)) !! (x) ) == Arvore ) = Jogador (x,y)
    | (j == Move Direita) && (( snd ( p !! (y)) !! (x+1) ) == Arvore ) = Jogador (x,y)
    | (j == Move Esquerda) && (( snd ( p !! (y)) !! (x-1) ) == Arvore ) = Jogador (x,y)
    | otherwise = moveJogador (Jogador (x,y)) j (Mapa l p)


-- Funçao que faz o jogador mover-se com base nas suas coordenadas e no terreno em que ele se encontra caso seja permitido.

{- | =moveJogador
* A funcao moveJogador faz o jogador mover-se com base nas suas coordenadas e no terreno em que ele se encontra caso seja permitido.
Esta funcao serve como auxiliar para a funcao jogadaValida.
== Corpo da função:

@
moveJogador :: Jogador -> Jogada -> Mapa -> Jogador
moveJogador (Jogador (x,y)) Parado (Mapa l p) 
    | ( snd ( p !! y ) !! (x) ) == Tronco = Jogador (x + getVelocidade ( fst (p !! y ) ), y) 
    | otherwise = Jogador (x,y)
moveJogador (Jogador (x,y)) (Move Cima) (Mapa l p) = if ( snd ( p !! (y-1)) !! (x) ) == Arvore then Jogador (x,y) else Jogador (x, y-1)
moveJogador (Jogador (x,y)) (Move Baixo) (Mapa l p) = if ( snd ( p !! (y+1) ) !! (x) ) == Arvore then Jogador (x,y) else Jogador (x, y+1) 
moveJogador (Jogador (x,y)) (Move Esquerda) (Mapa l p) = if ( snd ( p !! (y) ) !! (x-1) ) == Arvore then Jogador (x,y) else Jogador (x-1, y)
moveJogador (Jogador (x,y)) (Move Direita) (Mapa l p) = if ( snd ( p !! (y+1) ) !! (x) ) == Arvore then Jogador (x,y) else Jogador (x+1, y)

@

== Exemplos de utilização:
>>> getVelocidade (Rio 1)
1
== Funcoes auxiliares
@
myFst :: Jogador -> Int 
myFst (Jogador (x,y)) = x
mySnd :: Jogador -> Int 
mySnd (Jogador (x,y)) = y
@
 -}


moveJogador :: Jogador -> Jogada -> Mapa -> Jogador
moveJogador (Jogador (x,y)) Parado (Mapa l p)  = Jogador (x,y)
moveJogador (Jogador (x,y)) (Move Cima) (Mapa l p) =  Jogador (x, y-1)
moveJogador (Jogador (x,y)) (Move Baixo) (Mapa l p) =  Jogador (x, y+1) 
moveJogador (Jogador (x,y)) (Move Esquerda) (Mapa l p) =  Jogador (x-1, y)
moveJogador (Jogador (x,y)) (Move Direita) (Mapa l p) =  Jogador (x+1, y)


-- Funcoes auxiliares
myFst :: Jogador -> Int 
myFst (Jogador (x,y)) = x
mySnd :: Jogador -> Int 
mySnd (Jogador (x,y)) = y

isRelva :: Terreno -> Bool
isRelva Relva = True
isRelva _ = False 

