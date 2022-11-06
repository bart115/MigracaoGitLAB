{- |
Module      : Tarefa1_2022li1g029
Description : Validação de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g029 where

import LI12223

--1--
mapaValido :: Mapa -> Bool
mapaValido (Mapa lr l) = mapaValido1 (Mapa lr l)

mapaValido1 :: Mapa -> Bool
mapaValido1 (Mapa lr []) = True
mapaValido1 (Mapa lr ((Relva,lo):t)) 
        |(elem Carro lo || elem Tronco lo) = False  
        |otherwise = mapaValido1 (Mapa lr t)
 
mapaValido1 (Mapa lr (((Rio vr),lo):t))
        |(elem Carro lo || elem Arvore lo) = False 
        |otherwise = mapaValido1 (Mapa lr t)
        
mapaValido1 (Mapa lr (((Estrada ve),lo):t))
        |(elem Arvore lo || elem Tronco lo) = False
        |otherwise = mapaValido1 (Mapa lr t)

--2--
mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa lr []) = True  
mapaValido2 (Mapa lr (((Rio vr),lo): ((Rio vr1),lo1) :t))
                |(vr > 0 && vr1 > 0) || (vr < 0 && vr1 < 0) || (vr == 0 && vr1 == 0) = False
                |otherwise = mapaValido2 (Mapa lr ((Rio vr1),lo1):t)


