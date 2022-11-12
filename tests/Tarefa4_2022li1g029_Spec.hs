module Tarefa4_2022li1g029_Spec where

import LI12223
import Tarefa4_2022li1g029
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [ "Teste1,se o jogador saiu pelo lado direito do mapa na Relva" ~: True  ~=? jogoTerminou1 (Jogo (Jogador (4,4)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore])]))
                                              ,"Teste1.2,se o jogador saiu pelo lado direito do mapa na Estrada" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,5)) (Mapa 4 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste1.3,se o jogador saiu pelo lado direito do mapa no Rio" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,5)) (Mapa 4 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]))
                                              ,"Teste1',se o jogador não saiu pelo lado direito do mapa" ~: False ~=? jogoTerminou1 (Jogo (Jogador (4,1)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore])]))   
                                              ,"Teste2,se o jogador saiu pelo lado esquerdo do mapa na Relva" ~: True ~=? jogoTerminou1 (Jogo (Jogador ((-1),1)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore])]))
                                              ,"Teste2.1,se o jogador saiu pelo lado esquerdo do mapa no Rio" ~: True ~=? jogoTerminou1 (Jogo (Jogador ((-1),1)) (Mapa 5 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]))
                                              ,"Teste2.2,se o jogador saiu pelo lado esquerdo do mapa na Estrada" ~: True ~=? jogoTerminou1 (Jogo (Jogador ((-1),1)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste3,se o jogador não for rápido o suficiente,por baixo,Relva" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,4)) (Mapa 5 [(Relva ,[Nenhum,Nenhum,Arvore])]))
                                              ,"Teste3.1,se o jogador não for rápido o suficiente,por baixo,Rio" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,4)) (Mapa 5 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]))
                                              ,"Teste3.2, se o jogador não for rápido o suficiente,por baixo,Estrada" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,4)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste3' , se o jogador não sair por baixo" ~: False ~=? jogoTerminou1 (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 5,[Nenhum,Carro,Carro])]))]



