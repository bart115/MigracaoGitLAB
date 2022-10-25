module Tarefa3_2022li1g029_Spec where

import LI12223
import Tarefa3_2022li1g029
import Test.HUnit

testsT3 :: Test
testsT3 = TestLabel "Testes Tarefa 3" $ test ["Teste 1" ~: 1 ~=? 1]
