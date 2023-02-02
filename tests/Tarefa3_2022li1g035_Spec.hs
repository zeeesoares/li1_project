module Tarefa3_2022li1g035_Spec where

import LI12223
import Tarefa3_2022li1g035
import Test.HUnit
-- (Jogo (Jogador (x,y)) (Mapa l ((terreno,obs):t))) j
testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ 
    test ["Teste 1" ~: Jogo (Jogador (2,0)) (Mapa 4 [(Estrada 1, [Nenhum, Nenhum, Carro, Nenhum]), (Rio 1, [Nenhum, Tronco , Tronco , Nenhum]), 
    (Relva, [Nenhum, Arvore, Nenhum, Nenhum])])  
    ~=? animaJogo (Jogo (Jogador (2,1)) (Mapa 4 [(Estrada 1, [Nenhum, Nenhum, Carro, Nenhum]), (Rio 1, [Nenhum, Tronco , Tronco , Nenhum]), 
    (Relva, [Nenhum, Arvore, Nenhum, Nenhum]) ])) (Move Cima),
    "Teste 2" ~: Jogo (Jogador (2,0)) (Mapa 4 [ (Rio 1, [Nenhum, Nenhum, Tronco , Tronco]), 
    (Relva, [Nenhum, Arvore, Nenhum, Nenhum]),(Estrada 1, [Nenhum, Nenhum, Nenhum, Carro])])  
    ~=? animaJogo (Jogo (Jogador (2,1)) (Mapa 4 [ (Rio 1, [Nenhum, Tronco , Tronco , Nenhum]), 
    (Relva, [Nenhum, Arvore, Nenhum, Nenhum]), (Estrada 1, [Nenhum, Nenhum, Carro, Nenhum]) ])) (Move Cima),
    "Teste 3" ~: Jogo (Jogador (2,0)) (Mapa 4 [ (Rio 1, [Nenhum, Nenhum, Tronco , Tronco]), 
    (Relva, [Nenhum, Arvore, Nenhum, Nenhum]),(Rio 1, [Tronco, Nenhum , Tronco , Tronco])])  
    ~=? animaJogo (Jogo (Jogador (2,1)) (Mapa 4 [ (Rio 1, [Nenhum, Tronco , Tronco , Nenhum]), 
    (Relva, [Nenhum, Arvore, Nenhum, Nenhum]), (Rio 1, [Nenhum, Tronco , Tronco , Tronco]) ])) (Move Cima),
    "Teste 4" ~: Jogo (Jogador (1,1)) (Mapa 4 [ (Relva, [Nenhum, Arvore, Arvore , Nenhum]), 
    (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])  
    ~=? animaJogo (Jogo (Jogador (1,1)) (Mapa 4 [ (Relva, [Nenhum, Arvore, Arvore , Nenhum]), 
    (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])) (Move Cima),
    "Teste 5" ~: Jogo (Jogador (4,0)) (Mapa 4 [ (Rio 1, [Tronco, Nenhum, Tronco, Nenhum]), (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])  
    ~=? animaJogo (Jogo (Jogador (3,0)) (Mapa 4 [ (Rio 1, [Nenhum, Tronco, Nenhum, Tronco]), 
    (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])) (Parado),
    "Teste 6" ~: Jogo (Jogador (3,0)) (Mapa 4 [(Relva, [Arvore, Nenhum, Nenhum, Nenhum]), 
    (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])  
    ~=? animaJogo (Jogo (Jogador (3,0)) (Mapa 4 [ (Relva, [Arvore, Nenhum, Nenhum, Nenhum]), 
    (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])) (Move Direita),
    "Teste 7" ~: Jogo (Jogador (0,0)) (Mapa 4 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum]),
    (Relva,[Arvore,Nenhum,Nenhum,Nenhum])]) 
    ~=? animaJogo (Jogo (Jogador (0,0)) (Mapa 4 [ (Relva, [Nenhum, Arvore, Nenhum, Nenhum]), 
    (Relva, [Arvore, Nenhum, Nenhum, Nenhum])])) (Move Esquerda),
    "Teste 8" ~: Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 1, [Nenhum, Carro, Nenhum])])
    ~=? (animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 3, [Carro, Nenhum, Nenhum])]) ) Parado) ,
     "Teste 9" ~: Jogo (Jogador (1,0)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Tronco, Nenhum, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]) 
    ~=? (animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Relva, [Nenhum, Nenhum, Nenhum]),(Rio 1, [Nenhum, Tronco, Tronco]),(Relva, [Nenhum, Nenhum, Nenhum])]) ) (Move Cima))

    
    
    ]

