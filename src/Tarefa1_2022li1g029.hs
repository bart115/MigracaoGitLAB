{- |
Module      : Tarefa1_2022li1g029
Description : Validação de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g029 where

import LI12223

mapaValido :: Mapa -> Bool
mapaValido (Mapa lr ((tr,lo):t)) 
                        |tr == Relva = not (elem Carro lo || elem Tronco lo)  
                        |otherwise = True 

mapaValido (Mapa lr ((Rio v),lo):t)
                        |(elem Carro lo || elem Tronco lo) = False
                        |otherwise = True 


