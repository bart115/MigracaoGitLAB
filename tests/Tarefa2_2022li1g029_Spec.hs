module Tarefa2_2022li1g029_Spec where

import LI12223
import Tarefa2_2022li1g029
import Test.HUnit

testsT2 :: Test
testsT2 = TestLabel "Testes Tarefa 2" $ test [ "Teste 1" ~: estendeMapa (Mapa 4 [(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum])]) 16 ~=? Mapa 4 [(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum]),(Estrada 0,[Nenhum,Carro,Carro,Carro])],
                                               "Teste 2" ~: estendeMapa (Mapa 7 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum])]) 25 ~=? Mapa 7 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum]),(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])],
                                               "Teste 3" ~: estendeMapa (Mapa 6 [(Estrada 1,[Carro,Carro,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum,Carro,Carro]),(Estrada 1,[Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro])]) 3 ~=? Mapa 6 [(Estrada 1,[Carro,Carro,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum,Carro,Carro]),(Estrada 1,[Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])] ]
                                             

