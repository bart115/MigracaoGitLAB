{- |
Module      : Tarefa5_2022li1g029
Description : Movimentação do personagem e obstáculos
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g029 where

import LI12223
import Tarefa2_2022li1g029 



{-|A função deslizaJogo serve simplesmente para estender o mapa e ao mesmo tempo retirar a ultima linha do jogo


-}
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) (Mapa l t)) = Jogo (Jogador (x,y+1)) (deslizaux (estendeMapa (Mapa l t) seed )) 
                                                                            where 
                                                                                deslizaux (Mapa l t) = Mapa l (init t)





