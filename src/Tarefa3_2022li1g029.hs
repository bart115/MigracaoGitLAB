{- |
Module      : Tarefa3_2022li1g029
Description : Movimentação do personagem e obstáculos
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g029 where

import LI12223

{- |A função 'animaJogo' dá movimento tanto ao jogador como aos obstaculos do jogo.
A função usa outras 2 funções auxiliares.
== A função está preparada para funcionar em casos especificos como:

*A jogada do jogador é simplemente ficar parado.
+Se a jogada do jogador é ficar parado e o jogador está em cima de um tronco, o jogador acompanha o movimento do tronco
+Se o jogador chegou ao limite do mapa , mesmo que receba a jogada para o jogador se mover para cima, o jogador fica no mesmo sitio

==Exemplos de utilização

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]))´Parado
Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])]))´Parado
Jogo (Jogador (2,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (1,2)) (Mapa 4[(Rio (-1),[Tronco,Nenhum,Nenhum,Tronco),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])]))
Jogo (Jogador (1,2)) (Mapa 4[(Rio (-1),[Nenhum,Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])])

-}


{-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((Relva,ob1:obs):tf))) m = Jogo (Jogador (pos (x,y) m (Mapa l ((Relva,ob1:obs):tf)))) (Mapa l (mapamove ((Relva,ob1:obs):tf)))
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((Estrada v,ob1:obs):tf))) m = Jogo (Jogador (pos (x,y) m (Mapa l ((Estrada v,ob1:obs):tf)))) (Mapa l (mapamove ((Estrada v,ob1:obs):tf)))
animaJogo (Jogo (Jogador (x,y)) (Mapa l ((Rio v,ob1:obs):tf))) m = Jogo (Jogador (pos (x,y) m  (Mapa l ((Rio v,ob1:obs):tf)))) (Mapa l (mapamove ((Rio v,ob1:obs):tf))) -}                                  
{-}

pos::(Int,Int)->Jogada->Mapa->(Int,Int)
pos (x,y) Parado (Mapa l []) = (x,y)
pos (x,y) Parado (Mapa l ((Rio v,ob1:obs):tf)) |hatronco x (posline y (Mapa l ((Rio v,ob1:obs):tf)))==True = (x+v,y)
                                               |otherwise =(x,y)


-- |checktronco x (checkline (Jogo (Jogador (x,y)) (Mapa l ((Rio v,ob1:obs):tf)))) == True = (x+v,y)
--                                               |otherwise = pos (x,y+1) Parado (Mapa l tf)
pos (x,y) Parado (Mapa l ((te,ob1):obs))= (x,y)
pos (x,y) (Move Cima) (Mapa l ((ob1:obs)))|numfilas (Mapa l (ob1:obs)) > y = (x,y+1)
                                          |otherwise = (x,y)
pos (x,y) (Move Baixo) t =(x,y-1)
pos (x,y) (Move Esquerda) t =(x-1,y)
pos (x,y) (Move Direita) t = (x+1,y)
-}

--pos::(Int,Int)->Jogada->Mapa->(Int,Int)
--pos (x,y) Parado (Mapa l (Rio v,obs):tf) |checktronco x checkline 
{-animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m       = Jogo (Jogador (pos (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) Parado)) (Mapa lar (mapamove ((te,obs):tf)))
                                         where 
                                            pos::Jogo->Jogada->(Int,Int)
                                            pos (Jogo (Jogador (x,y)) (Mapa lar [])) Parado = (x,y)
                                            pos (Jogo (Jogador (x,y)) (Mapa lar ((Rio v,obs):tf))) Parado |y==0 && hatronco x obs == True = (x+v,y)
                                                                                                          |otherwise = pos (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
                                            pos (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) Parado = pos (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
                                            pos (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Cima) |numfilas (Mapa lar ((te,obs):tf)) > y = (x,y+1)
                                                                                                            |otherwise = (x,y)
                                            pos (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Baixo)  =(x,y-1)
                                            pos (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Esquerda) =(x-1,y)
                                            pos (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Direita)= (x+1,y)
-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m       = Jogo (Jogador ((posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m),(posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m ))) (Mapa lar (mapamove ((te,obs):tf)))
                                         where 
                                            posx::Jogo->Jogada->Int
                                            posx (Jogo (Jogador (x,y)) (Mapa lar [])) Parado = x
                                            posx (Jogo (Jogador (x,y)) (Mapa lar ((Rio v,obs):tf))) Parado |y==0 && hatronco x obs == True = x+v
                                                                                                           |otherwise = posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
                                            posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) Parado = x
                                            posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Esquerda) =x-1
                                            posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Direita)= x+1
                                            posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m  =x
                                            
posy::Jogo->Jogada->Int 
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Baixo)  =(y-1)
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Cima) |numfilas (Mapa lar ((te,obs):tf)) > y =(y+1)
                                                                 |otherwise = y
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m = y



--posline::Int->Mapa ->[Obstaculo]
--posline y (Mapa l ((t1,obs):tf))|y==0=obs
--                                |otherwise= posline (y-1) (Mapa l tf)


hatronco::Int->[Obstaculo]->Bool
hatronco 0 [] =False
hatronco x (ob1:obs) |x==0 && ob1==Tronco=True
                     |otherwise = hatronco (x-1) obs




{-}


checkline::Jogo->(Terreno,[Obstaculo])
checkline (Jogo (Jogador (x,0)) (Mapa l ((t,obs):tf)))=(t,obs)
checkline (Jogo (Jogador (x,y)) (Mapa l ((t,obs):tf))) = checkline (Jogo (Jogador (x,y-1))(Mapa l tf))
                                                     



checktronco::Int->(Terreno,[Obstaculo])->Bool
checktronco x (t,[])=False 
checktronco x (t,(ob1:obs)) |x==0 && ob1 == Tronco = True  
                            |otherwise = checktronco (x-1) (t,obs) 
                                                     
-}


mapamove::[(Terreno,[Obstaculo])]->[(Terreno,[Obstaculo])]
mapamove [(te,obs)] = [(te,obsmove (te,obs))]
mapamove ((te,obs):fs) = (te,obsmove (te,obs)):mapamove fs
               


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







                


