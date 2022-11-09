{- |
Module      : Tarefa3_2022li1g029
Description : Movimentação do personagem e obstáculos
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g029 where

import LI12223

animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((t,ob1:obs):tf))) m = Jogo (Jogador (pos (x,y) m t)) (Mapa l (mapamove ((t,ob1:obs):tf)))
                                               where
                                                  pos (x,y) Parado (Rio v ) |checktronco x (checkline (Jogo (Jogador (x,y)) (Mapa l ((t,ob1:obs):tf))))==True = (x+v,y)
                                                                            |otherwise = (x,y)
                                                                   
                                                  pos (x,y) (Move Cima) t = (x,y+1)
                                                  pos (x,y) (Move Baixo) t = (x,y-1)
                                                  pos (x,y) (Move Esquerda) t =(x-1,y)
                                                  pos (x,y) (Move Direita) t =(x+1,y)

checkline::Jogo->[Obstaculo]
checkline (Jogo (Jogador (x,y)) (Mapa l ((t,obs):tf))) |y==0 =obs
                                                       |otherwise = checkline (Jogo (Jogador (x,y-1))(Mapa l (tf)))
                                                     
checktronco::Int->[Obstaculo]->Bool
checktronco x (ob1:obs) |x==0 && ob1 == Tronco = True 
                        |x==0 && ob1 /= Tronco = False 
                        |otherwise = checktronco (x-1) obs 
                                                     



mapamove::[(Terreno,[Obstaculo])]->[(Terreno,[Obstaculo])]
mapamove [(t,obs)] = [(t,obsmove (t,obs))]
mapamove ((t,obs):fs) = (t,obsmove (t,obs)):mapamove fs
               


obsmove::(Terreno,[Obstaculo])->[Obstaculo]
obsmove (Relva,obs) = obs
obsmove (Rio v,obs)  |v==0 = obs
                     |v>0 = obsmove (Rio (v-1),(last obs : init obs ))
                     |otherwise= obsmove (Rio (v+1),tail obs ++ [head obs] )

obsmove (Estrada v,obs) |v==0 = obs
                        |v>0 = obsmove (Estrada (v-1),last obs: init obs )
                        |otherwise = obsmove (Estrada (v+1),tail obs ++ [head obs])







                


