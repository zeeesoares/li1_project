module Tarefa4_2022li1g035_Spec where

import LI12223
import Tarefa4_2022li1g035
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test 
    ["Teste 1" ~: True  ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 1, [Carro, Nenhum])])),
     "Teste 2" ~: True  ~=? jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Rio 1, [Nenhum, Tronco])])),
     "Teste 3" ~: True  ~=? jogoTerminou (Jogo (Jogador (-1,0)) (Mapa 2 [(Estrada 1, [Carro, Nenhum])])),
     "Teste 4" ~: True  ~=? jogoTerminou (Jogo (Jogador (0,-1)) (Mapa 2 [(Estrada 1, [Carro, Nenhum])])),
     "Teste 5" ~: False  ~=? jogoTerminou (Jogo (Jogador (1,0)) (Mapa 2 [(Estrada 1, [Carro, Nenhum])]))

    
    ]
