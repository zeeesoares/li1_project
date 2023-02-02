module Tarefa1_2022li1g035_Spec where

import LI12223
import Tarefa1_2022li1g035
import Test.HUnit

testsT1 :: Test
testsT1 = TestLabel "Testes Tarefa 1" $ 
    test [["mapaValido (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Estrada (-1),[Nenhum,Carro,Carro]), (Rio 2,[Tronco,Nenhum,Nenhum])])" ~: False ~=? mapaValido (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Estrada (-1),[Nenhum,Tronco,Tronco]), (Rio 2,[Tronco,Nenhum,Nenhum])]),
    "terrenoMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Estrada 1, [Carro,Carro,Nenhum,Carro])])" ~: True ~=? terrenoMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Estrada 1, [Carro,Carro,Nenhum,Carro])]),
    "terrenoMapa (Mapa 4 [(Rio (-1),[Tronco,Arvore,Nenhum,Tronco])])" ~: False ~=? terrenoMapa (Mapa 4 [(Rio (-1),[Tronco,Arvore,Nenhum,Tronco])]),
    "terrenoValido (Rio (-1),[Tronco,Tronco,Nenhum,Tronco])" ~: True ~=? terrenoValido (Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),
    "contiguosMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])])" ~: True ~=? contiguosMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])]),
    "troncoMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])])" ~: True ~=? troncoMapa (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])]),
    "troncoMapa (Mapa 7 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum])])" ~: False ~=? troncoMapa (Mapa 7 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum])]),
    "selectRios (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Estrada 1,[Carro,Carro,Nenhum,Nenhum])])" ~: [[Tronco,Tronco,Nenhum,Tronco]] ~=? selectRios (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco]),(Estrada 1,[Carro,Carro,Nenhum,Nenhum])]),
    "troncosValidos [Tronco,Tronco,Nenhum,Tronco]" ~: True ~=? troncosValidos [Tronco,Tronco,Nenhum,Tronco],
    "troncosValidos [Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]" ~: False ~=? troncosValidos [Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco],
    "troncosValidos [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]" ~: False ~=? troncosValidos [Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco],
    "contaTroncos [Tronco,Tronco,Nenhum,Tronco]" ~: [[Tronco,Tronco],[Tronco]] ~=? contaTroncos [Tronco,Tronco,Nenhum,Tronco],
    "isRio (Rio 1)"  ~: True ~=? isRio (Rio 1),
    "isRio (Estrada 2)" ~: False ~=? isRio (Estrada 2),
    "carroMapa (Mapa 4 [(Estrada (-1),[Carro,Carro,Nenhum,Carro]),(Estrada 1,[Carro,Carro,Nenhum,Nenhum])])" ~: True ~=? carroMapa (Mapa 4 [(Estrada (-1),[Carro,Carro,Nenhum,Carro]),(Estrada 1,[Carro,Carro,Nenhum,Nenhum])]),
    "carroMapa (Mapa 4 [(Estrada (-1),[Carro,Carro,Carro,Carro]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])])" ~: False ~=? carroMapa (Mapa 4 [(Estrada (-1),[Carro,Carro,Carro,Carro]),(Rio 1,[Tronco,Tronco,Nenhum,Nenhum])]),
    "carrosValidos [Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro]" ~: True ~=? carrosValidos [Nenhum,Carro,Nenhum,Carro,Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro],
    "contaObs [Tronco,Tronco,Nenhum,Tronco]"  ~: [[Tronco,Tronco],[Nenhum],[Tronco]] ~=? contaObs [Tronco,Tronco,Nenhum,Tronco],
    "linhaValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])])" ~: True ~=? linhaValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])]),
    "linhaValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Tronco,Tronco])])" ~: False ~=? linhaValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Tronco,Tronco])]),
    "larguraValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])])" ~: True ~=? larguraValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Nenhum,Tronco])]),
    "larguraValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Tronco,Tronco])])" ~: True ~=? larguraValida (Mapa 4 [(Rio (-1),[Tronco,Tronco,Tronco,Tronco])]),
    "terrenosSucessivos (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Estrada (-1),[Nenhum,Carro,Carro]), (Rio 2,[Tronco,Nenhum,Nenhum])])"  ~: True ~=? terrenosSucessivos (Mapa 3 [(Rio 1,[Nenhum,Tronco,Tronco]),(Estrada (-1),[Nenhum,Carro,Carro]), (Rio 2,[Tronco,Nenhum,Nenhum])]),
    "terrenosSucessivos (Mapa 1 [(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum])])"  ~: False ~=? terrenosSucessivos (Mapa 1 [(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum])])
    ]]



-- usar esta linha --> ghci -i="src" -i="tests" tests/Tarefa1_2022li1g035_Spec.hs src/Tarefa1_2022li1g035.hs; runTestTT testsT1