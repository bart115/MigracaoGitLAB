module Tarefa4_2022li1g029_Spec where

import LI12223
import Tarefa4_2022li1g029
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test [ "Teste 1,se o jogador saiu pelo lado direito do mapa na Relva" ~: True  ~=? jogoTerminou1 (Jogo (Jogador (4,4)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore])]))
                                              ,"Teste 1.2,se o jogador saiu pelo lado direito do mapa na Estrada" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,5)) (Mapa 4 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste 1.3,se o jogador saiu pelo lado direito do mapa no Rio" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,5)) (Mapa 4 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]))
                                              ,"Teste 1',se o jogador não saiu pelo lado direito do mapa" ~: False ~=? jogoTerminou1 (Jogo (Jogador (4,1)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore]),(Rio 4,[Nenhum,Nenhum,Tronco])]))   
                                              ,"Teste 2,se o jogador saiu pelo lado esquerdo do mapa na Relva" ~: True ~=? jogoTerminou1 (Jogo (Jogador ((-1),1)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore])]))
                                              ,"Teste 2.1,se o jogador saiu pelo lado esquerdo do mapa no Rio" ~: True ~=? jogoTerminou1 (Jogo (Jogador ((-1),1)) (Mapa 5 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]))
                                              ,"Teste 2.2,se o jogador saiu pelo lado esquerdo do mapa na Estrada" ~: True ~=? jogoTerminou1 (Jogo (Jogador ((-1),1)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste 3,se o jogador não for rápido o suficiente,por baixo,Relva" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,4)) (Mapa 5 [(Relva ,[Nenhum,Nenhum,Arvore])]))
                                              ,"Teste 3.1,se o jogador não for rápido o suficiente,por baixo,Rio" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,4)) (Mapa 5 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]))
                                              ,"Teste 3.2, se o jogador não for rápido o suficiente,por baixo,Estrada" ~: True ~=? jogoTerminou1 (Jogo (Jogador (5,4)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste 3',se o jogador não sair por baixo" ~: False ~=? jogoTerminou1 (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Arvore,Nenhum]),(Estrada 5,[Nenhum,Carro,Carro])]))
                                              ,"Teste 4, se o jogador caiu no Rio" ~: True ~=? jogoTerminou2 (Jogo (Jogador (2,0)) (Mapa 5 [(Rio 3 ,[Tronco,Tronco,Nenhum])]))
                                              ,"Teste 4.1, se o jogador caiu no Rio noutra linha " ~: True ~=? jogoTerminou2 (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Rio 3,[Tronco,Tronco,Nenhum])]))
                                              ,"Teste 4.2, se o jogador não caiu no rio" ~: False ~=? jogoTerminou2 (Jogo (Jogador (1,0)) (Mapa 5 [(Rio 3 ,[Tronco,Tronco,Nenhum])]))
                                              ,"Teste 4.3, se o jogador não caiu no rio noutra linha" ~: False ~=? jogoTerminou2 (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Rio 3,[Tronco,Nenhum,Tronco])]))
                                              ,"Teste 5 ,se o jogador foi atropelado" ~: True ~=? jogoTerminou3 (Jogo (Jogador (2,0)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste 5.1, se o jogador foi atropelado noutra linha" ~: True ~=? jogoTerminou3 (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste 5.2, se o jogador não foi atropelado " ~: False ~=? jogoTerminou3 (Jogo (Jogador (1,0)) (Mapa 5 [(Estrada 3 ,[Carro,Nenhum,Nenhum])]))
                                              ,"Teste 5.3, se o jogador não foi atropelado noutra linha" ~: False ~=? jogoTerminou3 (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Estrada 3,[Tronco,Nenhum,Nenhum])])) 
                                              ,"Teste 6, teste para a função _jogoTerminou_ com mais que uma linha " ~: True ~=? jogoTerminou (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Nenhum,Carro])]))
                                              ,"Teste 6.1, teste para a função _jogoTerminou_ " ~: True ~=? jogoTerminou (Jogo (Jogador ((-1),1)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro])]))]



