module Tarefa4_2022li1g029_Spec where

import LI12223
import Tarefa4_2022li1g029
import Test.HUnit

testsT4 :: Test
testsT4 = TestLabel "Testes Tarefa 4" $ test ["Teste 1" ~: 1 ~=? 1]
