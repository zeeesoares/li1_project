module Tarefa2_2022li1g035_Spec where

import LI12223
import Tarefa2_2022li1g035
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $
    test 
  [["estendeMapa (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) 12" ~: Mapa 3 [(Rio (-2),[Tronco,Nenhum,Tronco]),(Relva,[Arvore,Arvore,Nenhum])] ~=? estendeMapa (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) 12,
    "proximoTerreno 12 (Mapa 10 [])" ~: (Rio (-2),[]) ~=? proximoTerreno 12 (Mapa 10 []),
    "juntaTudo 12 (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) (Rio 0,[])" ~: (Rio 0,[Tronco,Nenhum,Tronco]) ~=? juntaTudo 12 (Mapa 3 [(Relva,[Arvore,Arvore,Nenhum])]) (Rio 0,[])
                
    ]]
