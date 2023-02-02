module Tarefa5_2022li1g035_Spec where


import LI12223
import Tarefa5_2022li1g035
import Test.HUnit

testsT5 :: Test
testsT5 = TestLabel "Testes Tarefa 5" $ test 
    ["Teste 1" ~: Jogo (Jogador (0,1)) (Mapa 2 [(Rio 1,[Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum])])  ~=? deslizaJogo (Jogo (Jogador (0,0)) (Mapa 2 [(Estrada 1, [Carro, Nenhum]),(Relva,[Nenhum,Nenhum])])) 1,
     "Teste 2" ~: Mapa 2 [(Rio 1,[Nenhum,Nenhum])]  ~=? eliminaMapa (Mapa 2 [(Rio 1,[Nenhum,Nenhum]),(Estrada 1,[Carro,Nenhum])]),
     "Teste 3" ~: Jogador (0,2)  ~=? moveJogadorDeslize (Jogador (0,1))
     
    ]
