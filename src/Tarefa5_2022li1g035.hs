{- |
Module      : Tarefa5_2022li1g035
Description : Determinar se o jogo terminou
Copyright   : José António Costa Soares <a103995@alunos.uminho.pt>
              Nuno Miguel Parente Morais <a104090@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}

module Tarefa5_2022li1g035 where
import LI12223
import Tarefa2_2022li1g035

{- | =deslizaJogo
* A função que apaga o inicio de um mapa e de seguida adiciona uma nova linha, sendo que o jogador deve avançar 1 unidade no y

@
deslizaJogo :: Jogo -> Int -> Jogo             -- função que apaga o inicio de um mapa e de seguida adiciona uma nova linha, sendo que o jogador deve avançar 1 unidade no y
deslizaJogo (Jogo j@(Jogador (x,y)) m@(Mapa l p)) n = Jogo (moveJogadorDeslize j) (estendeMapa (eliminaMapa m) n) 
@

== Exemplos de utilização:
>>> deslizaJogo (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 1, [Carro, Nenhum]),(Relva,[Nenhum,Nenhum])])) 1
Jogo (Jogador (0,1)) (Mapa 2 [(Rio 1,[Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum])])

-}

deslizaJogo :: Jogo -> Int -> Jogo            
deslizaJogo (Jogo j@(Jogador (x,y)) m@(Mapa l p)) n = Jogo (moveJogadorDeslize j) (estendeMapa (eliminaMapa m) n) 

eliminaMapa :: Mapa -> Mapa -- função que elimina o fim de um mapa
eliminaMapa (Mapa l lista) = Mapa l $ init lista

moveJogadorDeslize :: Jogador -> Jogador  -- função que aplica o efeito de deslize ao jogador
moveJogadorDeslize (Jogador (x,y)) = Jogador (x,y+1)
