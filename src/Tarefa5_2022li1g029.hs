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

==Exeplos de Utilização :
>>> deslizajogo 2 (Jogo (Jogador (2,2)) (Mapa 3 [(Relva ,[Nenhum,Arvore,Arvore]),(Relva ,[Nenhum,Nenhum,Arvore]),(Relva ,[Arvore,Nenhum,Arvore])]))
>>> (Jogo (Jogador (2,3)) (Mapa 3 [(Rio 3, [Nenhum,Nenhum,Tronco]),(Relva ,[Nenhum,Arvore,Arvore]),(Relva ,[Nenhum,Nenhum,Arvore])]))

-}
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo seed (Jogo (Jogador (x,y)) (Mapa l t)) = Jogo (Jogador (x,y+1))  (estendeMapa (Mapa l (init t)) seed )
                                                                            




