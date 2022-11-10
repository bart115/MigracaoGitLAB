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
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((Relva,ob1:obs):tf))) m = Jogo (Jogador (pos (x,y) m (Mapa l ((Relva,ob1:obs):tf)))) (Mapa l (mapamove ((Relva,ob1:obs):tf)))
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,ob1:obs):tf))) m = Jogo (Jogador (pos (x,y) m (Mapa l ((Estrada v,ob1:obs):tf)))) (Mapa l (mapamove ((Estrada v,ob1:obs):tf)))
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((Rio v,ob1:obs):tf))) m = Jogo (Jogador (pos (x,y) m  (Mapa l ((Rio v,ob1:obs):tf)))) (Mapa l (mapamove ((Rio v,ob1:obs):tf)))
                                    


pos::(Int,Int)->Jogada->Mapa->(Int,Int)
pos (x,y) Parado (Mapa l ((Rio v,[]):t))=(x,y)
pos (x,y) Parado (Mapa l ((Rio v,ob1:obs):tf)) |checktronco x (checkline (Jogo (Jogador (x,y)) (Mapa l ((Rio v,ob1:obs):tf))))==True = (x+v,y)
                                               |otherwise = (x,y)
pos (x,y) Parado (Mapa l ((te,ob1):obs))= (x,y)
pos (x,y) (Move Cima) (Mapa l ((ob1:obs)))|numfilas (Mapa l (ob1:obs)) > y = (x,y+1)
                                          |otherwise = (x,y)
pos (x,y) (Move Baixo) t =(x,y-1)
pos (x,y) (Move Esquerda) t =(x-1,y)
pos (x,y) (Move Direita) t = (x+1,y)



checkline::Jogo->[Obstaculo]
checkline (Jogo (Jogador (x,0)) (Mapa l []))=[]
checkline (Jogo (Jogador (x,0)) (Mapa l ((t,obs):tf)))=obs
checkline (Jogo (Jogador (x,y)) (Mapa l ((t,obs):tf))) = checkline (Jogo (Jogador (x,y-1))(Mapa l tf))
                                                     



checktronco::Int->[Obstaculo]->Bool
checktronco x []=False 
checktronco x (ob1:obs) |x==0 && ob1 == Tronco = True  
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



numfilas::Mapa->Int
numfilas (Mapa l [])=0
numfilas (Mapa l (ob1:obs))=1+numfilas (Mapa l obs)







                


