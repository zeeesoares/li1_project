{- |
Module      : Tarefa4_2022li1g035
Description : Determinar se o jogo terminou
Copyright   : José António Costa Soares <a103995@alunos.uminho.pt>
              Nuno Miguel Parente Morais <a104090@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g035 where

import LI12223
import Tarefa1_2022li1g035

{- | =jogoTerminou
* A funcao jogoTerminou verifica se depois da realização de uma jogada o jogador está numa posição em que pode continuar ou não.

@
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l p))
    | (fst (x,y) > l) || fst (x,y) < 0 = True 
    | snd (x,y) > (length p) || snd (x,y) < 0  = True
    | (isRio ((fst ( p !! (y)) )) == True) && (( snd ( p !! (y)) !! (x) ) == Nenhum) = True 
    | (isEstrada (fst ( p !! (y))) == True) && (( snd ( p !! (y)) !! (x) ) == Carro) = True 
    | otherwise = False
@

== Exemplos de utilização:
>>> jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 1, [Carro, Nenhum])]))
True

-}

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa l p))
    | (fst (x,y) > l-1) || fst (x,y) < 0 = True 
    | snd (x,y) > (length p) || snd (x,y) < 0  = True
    | (isRio ((fst ( p !! (y)) )) == True) && (( snd ( p !! (y)) !! (x) ) == Nenhum) = True 
    | (isEstrada (fst ( p !! (y))) == True) && (( snd ( p !! (y)) !! (x) ) == Carro) = True 
    | otherwise = False


