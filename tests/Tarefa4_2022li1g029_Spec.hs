module Tarefa4_2022li1g029_Spec where

import LI12223
import Tarefa4_2022li1g029
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste1,se o jogador saiu pelo lado direito do mapa na Relva" ~: True  ~=? jogoTerminou1 (Jogo (Jogador (1,5)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore])]))]



