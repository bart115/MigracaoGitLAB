module Tarefa3_2022li1g029_Spec where

import LI12223
import Tarefa3_2022li1g029
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1" ~:animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) Parado ~=?  Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]),
                                              "Teste 2" ~:animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) Parado ~=? Jogo (Jogador (2,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]),
                                              "Teste 3" ~:animaJogo (Jogo (Jogador (1,2)) (Mapa 4[(Rio (-1),[Tronco,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])])) (Move Cima) ~=? Jogo (Jogador (1,1)) (Mapa 4 [(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])]),
                                              "Teste 4" ~:animaJogo (Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Cima) ~=? Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]),
                                              "Teste 5" ~:animaJogo (Jogo (Jogador (2,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Baixo) ~=?  Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]),
                                              "Teste 6" ~:animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Esquerda) ~=? Jogo (Jogador (1,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]), 
                                              "Teste 7" ~:animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Direita) ~=? Jogo (Jogador (3,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]),
                                              "Teste 8" ~:animaJogo (Jogo (Jogador (1,0)) (Mapa 5 [(Rio 1,[Tronco,Tronco,Nenhum,Tronco,Nenhum])])) Parado ~=? Jogo (Jogador (2,0)) (Mapa 5 [(Rio 1,[Nenhum,Tronco,Tronco,Nenhum,Tronco])])]





