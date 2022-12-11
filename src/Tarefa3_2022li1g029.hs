{- |
Module      : Tarefa3_2022li1g029
Description : Movimentação do personagem e obstáculos
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g029 where

import LI12223

{- |Na realização desta tarefa criei uma série de funções auxiliares com o objetivo de respeitar as regras estabelecidas no enunciado

A função 'posx' calcula a posição em x após a jogada e está definida por: 

@
posx::Jogo->Jogada->Int
posx (Jogo (Jogador (x,y)) (Mapa lar [])) Parado = x
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) Parado = if y == 0 && hatronco x obs then  posaux te else posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
                                                                                          where posaux (Rio v) = x+v 
                                                                                                posaux te = x
posx (Jogo (Jogador (0,y)) (Mapa lar t)) (Move Esquerda) = 0
posx (Jogo (Jogador (x,y)) (Mapa lar [])) (Move Esquerda) = x-1 
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Esquerda) |y==0 && haarvore (x-1) obs = x
                                                                     |otherwise =  posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) (Move Esquerda) 
posx (Jogo (Jogador (x,y)) (Mapa lar [])) (Move Direita) = x+1
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Direita) |x==(lar-1)=x
                                                                    |y==0 && haarvore (x+1) obs = x
                                                                    |otherwise =  posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) (Move Direita) 
posx (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Cima) = x
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):(t2,ob2):tf))) (Move Cima) |y==1 && haarvore x obs && hatronco x ob2 = posaux t2
                                                                          |otherwise = posx (Jogo (Jogador (x,y-1)) (Mapa lar ((t2,ob2):tf))) (Move Cima)
                                                                           where posaux (Rio v) = x+v 
                                                                                 posaux te = x
posx (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Baixo)=x
posx (Jogo (Jogador (x,y)) (Mapa lar ((t1,ob1):(te,obs):tf))) (Move Baixo) |y==0 && haarvore x obs && hatronco x ob1 = posaux t1
                                                                           |otherwise = posx (Jogo (Jogador (x,y-1)) (Mapa lar ((te,obs):tf))) (Move Baixo) 
                                                                                 where posaux (Rio v) = x+v 
                                                                                       posaux te = x
@
-}

posx::Jogo->Jogada->Int
posx (Jogo (Jogador (x,y)) (Mapa lar [])) Parado = x
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) Parado |y == 0 && hatronco x obs =  posaux te
                                                            |otherwise= posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
                                                                                          where posaux (Rio v) = x+v 
                                                                                                posaux te = x
posx (Jogo (Jogador (x,y)) (Mapa lar [])) (Move Esquerda) = (x-1) 
posx (Jogo (Jogador (x,0)) (Mapa lar ((Rio v,obs):tf))) (Move Esquerda)=if hatronco x obs then x+v-1 else x-1  
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Esquerda) |y==0 && haarvore (x-1) obs = x
                                                                     |y==0 && x==0 = 0 --posaux te
                                                                     |otherwise =  posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) (Move Esquerda) 
                                                                                         --where posaux (Rio v) = v 
                                                                                         --      posaux te = 0
posx (Jogo (Jogador (x,y)) (Mapa lar [])) (Move Direita) = x+1
posx (Jogo (Jogador (x,0)) (Mapa lar ((Rio v,obs):tf))) (Move Direita)=if hatronco x obs then x+v+1 else x+1 
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Direita) |y==0 && haarvore (x+1) obs = x
                                                                    |y==0 && x==(lar-1)= x-- posaux te
                                                                    |otherwise =  posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) (Move Direita) 
                                                                                       -- where posaux (Rio v) = x+v+1 
                                                                                       --       posaux te = x
posx (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Cima) = x
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):(t2,ob2):tf))) (Move Cima) |y==1 && haarvore x obs && hatronco x ob2 = posaux t2
                                                                          |otherwise = posx (Jogo (Jogador (x,y-1)) (Mapa lar ((t2,ob2):tf))) (Move Cima)
                                                                           where posaux (Rio v) = x+v 
                                                                                 posaux te = x
posx (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Baixo)=x
posx (Jogo (Jogador (x,y)) (Mapa lar ((t1,ob1):(te,obs):tf))) (Move Baixo) |y==0 && haarvore x obs && hatronco x ob1 = posaux t1
                                                                           |otherwise = posx (Jogo (Jogador (x,y-1)) (Mapa lar ((te,obs):tf))) (Move Baixo) 
                                                                                 where posaux (Rio v) = x+v 
                                                                                       posaux te = x


{-|A função 'posy' calcula a posição em y após a jogada e está definida por:

@
posy::Int->Jogo->Jogada->Int 
posy a (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Baixo) = a+y+1
posy a (Jogo (Jogador (x,y)) (Mapa lar (t1:(te,obs):tf))) (Move Baixo)|y==0 && haarvore x obs =a+y
                                                                      |otherwise =  posy (a+1) (Jogo (Jogador (x,y-1)) (Mapa lar ((te,obs):tf))) (Move Baixo) 


posy a (Jogo (Jogador (x,0)) (Mapa lar t)) (Move Cima) = 0
posy a (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Cima) = a+y-1
posy a (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):t2:tf))) (Move Cima) |y==1 && haarvore x obs = a+y 
                                                                      |otherwise = posy (a+1) (Jogo (Jogador (x,y-1)) (Mapa lar ((te,obs):tf))) (Move Cima) 
posy a (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m = y
@  
-}

posy::Int->Jogo->Jogada->Int 
posy a (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Baixo) = a+y+1
posy a (Jogo (Jogador (x,y)) (Mapa lar (t1:(te,obs):tf))) (Move Baixo)|y==0 && haarvore x obs =a+y
                                                                      |otherwise =  posy (a+1) (Jogo (Jogador (x,y-1)) (Mapa lar ((te,obs):tf))) (Move Baixo) 


posy a (Jogo (Jogador (x,y)) (Mapa lar [t])) (Move Cima) = if y+a==0 then 0 else a +y-1
posy a (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):t2:tf))) (Move Cima) |y==1 && haarvore x obs = a+y
                                                                      |otherwise = posy (a+1) (Jogo (Jogador (x,y-1)) (Mapa lar (t2:tf))) (Move Cima) 
posy a (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m = y




{- |A proxima função auxiliar hatronco tem como função verificar se na posição do jogador se encontra um tronco

A função 'hatronco' está definida por:

@
hatronco::Int->[Obstaculo]->Bool
hatronco x [] =False
hatronco x (ob1:obs) |x==0 && ob1==Tronco=True
                     |otherwise = hatronco (x-1) obs
@


-}
hatronco::Int->[Obstaculo]->Bool
hatronco x [] =False
hatronco x (ob1:obs) |x==0 && ob1==Tronco=True
                     |otherwise = hatronco (x-1) obs
                  


{-|A função auxiliar haarvore permite-nos identificar a presença de uma árvore numa lista de obstáculos

A função 'haarvore está definida por:

@
haarvore::Int->[Obstaculo]->Bool
haarvore x [] =False
haarvore x (ob1:obs) |x==0 && ob1==Arvore=True
                     |otherwise = haarvore (x-1) obs
@

-}

haarvore::Int->[Obstaculo]->Bool
haarvore x [] =False
haarvore x (ob1:obs) |x==0 && ob1==Arvore=True
                     |otherwise = haarvore (x-1) obs

{- | Para dar movimento aos obstaculos fez-se as duas funções auxiliares 

A função 'obsmove' move os obstaculo de uma "linha" do jogo e está definida por:

@
obsmove::(Terreno,[Obstaculo])->[Obstaculo]
obsmove (Relva,obs) = obs
obsmove (Rio v,obs)  |v==0 = obs
                     |v>0 = obsmove (Rio (v-1),(last obs : init obs ))
                     |otherwise= obsmove (Rio (v+1),tail obs ++ [head obs] )
obsmove (Estrada v,obs) |v==0 = obs
                        |v>0 = obsmove (Estrada (v-1),last obs: init obs )
                        |otherwise = obsmove (Estrada (v+1),tail obs ++ [head obs])
@
-}
haCarro::Int->[Obstaculo]->Bool
haCarro x [] =False
haCarro x (ob1:obs) |x==0 && ob1==Carro=True
                    |otherwise = haCarro (x-1) obs 

obsmove::(Terreno,[Obstaculo])->[Obstaculo]
obsmove (Relva,obs) = obs
obsmove (Rio v,obs)  |v==0 = obs
                     |v>0 = obsmove (Rio (v-1),([last obs]++ init obs ))
                     |otherwise= obsmove (Rio (v+1),tail obs ++ [head obs] )
obsmove (Estrada v,obs) |v==0 = obs
                        |v>0 = obsmove (Estrada (v-1),[last obs ]++ init obs )
                        |otherwise = obsmove (Estrada (v+1),tail obs ++ [head obs])


obsmove2:: Int-> (Terreno,[Obstaculo])->Jogada->Int->[Obstaculo]
{-obsmove2 x (Estrada v,obs) (Move Cima) ac|v<0 = obsmove2 x (Estrada (v+1),tail obs++[head obs]) (Move Cima) ac
                                         |v>0 = obsmove2 x (Estrada (v-1),[last obs]++init obs) (Move Cima) ac
                                         |otherwise = obs 
obsmove2 x (Estrada v,obs) (Move Baixo) ac|v<0 = obsmove2 x (Estrada (v+1),tail obs++[head obs]) (Move Baixo) ac
                                          |v>0 = obsmove2 x (Estrada (v-1),[last obs]++init obs) (Move Baixo) ac
                                          |otherwise = obs-}
obsmove2 x (Estrada v,obs) (Move Cima) 0 |v<0 = obsmove2 x (Estrada (v+1),tail obs++[head obs]) (Move Cima) 1
                                         |v>0 = obsmove2 x (Estrada (v-1),[last obs]++init obs) (Move Cima) 1
                                         |otherwise = obs
obsmove2 x (Estrada v,obs) (Move Cima) ac = obsmove2 x (Estrada v,obs) (Parado) (ac+1) 
obsmove2 x (Estrada v,obs) (Move Baixo) 0|v<0 = obsmove2 x (Estrada (v+1),tail obs++[head obs]) (Move Baixo) 1
                                          |v>0 = obsmove2 x (Estrada (v-1),[last obs]++init obs) (Move Baixo) 1
                                          |otherwise = obs
obsmove2 x (Estrada v,obs) (Move Baixo) ac = obsmove2 x (Estrada v,obs) (Parado) (ac+1)
obsmove2 0 (Estrada v,obs) (Move Esquerda) ac= obsmove2 0 (Estrada v,obs) Parado ac                                          
obsmove2 x (Estrada v,obs) (Move Esquerda) ac|ac==0 && v<0 =  obsmove2 x (Estrada (v+1),tail obs++[head obs]) (Move Esquerda) (ac+1)
                                             |v<0 && haCarro (x-1) obs == False = obsmove2 x (Estrada (v+1),tail obs++[head obs]) (Move Esquerda) ac
                                             |v>0 && haCarro (x-1) obs == False = obsmove2 x (Estrada (v-1),[last obs]++init obs) (Move Esquerda) ac
                                             |otherwise = obs 
obsmove2 x (Estrada v,obs) (Move Direita) ac|x==((length obs)-1)= obsmove2 x (Estrada v,obs) Parado ac
                                            |ac==0  && v>0 = obsmove2 x (Estrada (v-1),[last obs]++init obs) (Move Direita) (ac+1)
                                            |v<0 && haCarro (x+1) obs == False = obsmove2 x (Estrada (v+1),tail obs++[head obs]) (Move Direita) ac
                                            |v>0 && haCarro (x+1) obs == False = obsmove2 x (Estrada (v-1),[last obs]++init obs) (Move Direita) ac
                                            |otherwise = obs  
obsmove2 x (Estrada v,obs) m ac|v<0 && haCarro x obs ==False = obsmove2 x (Estrada (v+1),tail obs++[head obs]) m ac
                               |v>0 && haCarro x obs ==False = obsmove2 x (Estrada (v-1),[last obs]++init obs) m ac
                               |otherwise = obs 

{- |A função 'mapamove' usa a função obsmove para mover todos os obstaculos e está definida por: 

@
mapamove::[(Terreno,[Obstaculo])]->[(Terreno,[Obstaculo])]
mapamove [(te,obs)] = [(te,obsmove (te,obs))]
mapamove ((te,obs):fs) = (te,obsmove (te,obs)):mapamove fs
@
-}
mapamove::[(Terreno,[Obstaculo])]->Int->Int->Jogada->Int->[(Terreno,[Obstaculo])]
mapamove [] x y m ac= []
mapamove ((Estrada v,obs):fs) x 0 (Move Cima) 0 =  (Estrada v,obsmove2 x (Estrada v,obs) Parado 0):mapamove fs x (-1) (Move Cima) 1
mapamove ((Estrada v,obs):(Estrada v1,obs2):fs) x 1 (Move Cima)  ac= (Estrada v,obsmove2 x (Estrada v,obs) (Move Cima) 0):(Estrada v1,obsmove (Estrada v1,obs2)):mapamove fs x (-1) (Move Cima) (ac+1)
mapamove ((Relva,obs):(Estrada v1,obs2):fs) x 1 (Move Cima) ac = if haarvore x obs then  (Relva,obs):(Estrada v1,obsmove2 x (Estrada v1,obs2) Parado 0): mapamove fs x (-1) (Move Cima) (ac+1) else (Relva,obs):(Estrada v1,obsmove(Estrada v1,obs2)): mapamove fs x (-1) (Move Cima) (ac+1)
mapamove ((Estrada v,obs):(Estrada v1,obs2):fs) x 0 (Move Baixo) ac= (Estrada v,obsmove (Estrada v,obs)):(Estrada v1,obsmove2 x (Estrada v1,obs2) (Move Baixo) 0):mapamove fs x (-2) (Move Baixo) (ac+1)
mapamove ((Estrada v,obs):(Relva,obs2):fs) x 0 (Move Baixo) ac = if haarvore x obs2 then (Estrada v,obsmove2 x (Estrada v,obs) Parado 0):(Relva,obs2):mapamove fs x (-1) (Move Baixo) (ac+1) else (Estrada v,obsmove(Estrada v,obs)):(Relva,obs2):mapamove fs x (-1) (Move Baixo) (ac+1)
mapamove ((Estrada v,obs):fs) x 0 (Move Cima) ac = ((Estrada v,obsmove (Estrada v,obs) ):mapamove fs x (-1) (Move Cima) (ac+1))
mapamove ((Estrada v,obs):fs) x 0 (Move Baixo) ac = ((Estrada v,obsmove (Estrada v,obs) ):mapamove fs x (-1) (Move Baixo) (ac+1))
mapamove ((Estrada v,obs):fs) x y m ac|y==0 =((Estrada v,obsmove2 x (Estrada v,obs) m 0):mapamove fs x (y-1) m (ac+1))
                                      |otherwise =((Estrada v,obsmove (Estrada v,obs)):mapamove fs x (y-1) m (ac+1))
mapamove ((te,obs):fs) x y m ac= (te,obsmove (te,obs)):(mapamove fs x (y-1) m (ac+1)) 

{-
mapamove::[(Terreno,[Obstaculo])]->Int->Int->Jogada->Int->[(Terreno,[Obstaculo])]
mapamove [] x y m ac= []
mapamove ((Estrada v,obs):fs) x 0 (Move Cima) 0 =  (Estrada v,obsmove2 x (Estrada v,obs) Parado 0):mapamove fs x (-1) (Move Cima) 1
mapamove ((Estrada v,obs):(Estrada v1,obs2):fs) x 1 (Move Cima)  ac= (Estrada v,obsmove2 x (Estrada v,obs) Parado 0):(Estrada v1,obsmove2 x (Estrada v1,obs2) (Move Cima) 0):mapamove fs x (-1) (Move Cima) (ac+1)
mapamove ((Relva,obs):(Estrada v1,obs2):fs) x 1 (Move Cima) ac = if haarvore x obs then  (Relva,obs):(Estrada v1,obsmove2 x (Estrada v1,obs2) Parado 0): mapamove fs x (-1) (Move Cima) (ac+1) else (Relva,obs):(Estrada v1,obsmove2 x (Estrada v1,obs2) (Move Cima) 0): mapamove fs x (-1) (Move Cima) (ac+1)
mapamove ((Estrada v,obs):(Estrada v1,obs2):fs) x 0 (Move Baixo) ac= (Estrada v,obsmove2 x (Estrada v,obs) (Move Baixo) 0):(Estrada v1,obsmove2 x (Estrada v1,obs2) Parado 0):mapamove fs x (-2) (Move Baixo) (ac+1)
mapamove ((Estrada v,obs):(Relva,obs2):fs) x 0 (Move Baixo) ac = if haarvore x obs2 then (Estrada v,obsmove2 x (Estrada v,obs) Parado 0):(Relva,obs2):mapamove fs x (-1) (Move Baixo) (ac+1) else (Estrada v,obsmove2 x (Estrada v,obs) (Move Baixo) 0):(Relva,obs2):mapamove fs x (-1) (Move Baixo) (ac+1)
mapamove ((Estrada v,obs):fs) x y m ac|y==0 =((Estrada v,obsmove2 x (Estrada v,obs) m 0):mapamove fs x (y-1) m (ac+1))
                                      |otherwise =((Estrada v,obsmove (Estrada v,obs)):mapamove fs x (y-1) m (ac+1))
mapamove ((te,obs):fs) x y m ac= (te,obsmove (te,obs)):(mapamove fs x (y-1) m (ac+1)) 
-}
               

{- |Finalmente,a função 'animaJogo' dá movimento tanto ao jogador como aos obstaculos do jogo.

A função está definida por:

@
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m  = Jogo (Jogador (px,py)) (Mapa lar (mapamove ((te,obs):tf)))
                                                                                     where px=posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m
                                                                                           py=posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m 
@


==A função está preparada para funcionar em casos especificos como:

*A jogada do jogador é simplemente ficar parado.
*Se a jogada do jogador é ficar parado e o jogador está em cima de um tronco, o jogador acompanha o movimento do tronco
*Se o jogador chegou ao limite do mapa , mesmo que receba a jogada para o jogador se mover para cima, o jogador fica no mesmo sitio

==Exemplos de utilização para os casos especificos

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) Parado
Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (1,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) Parado
Jogo (Jogador (2,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (1,0)) (Mapa 4[(Rio (-1),[Tronco,Nenhum,Nenhum,Tronco),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])])) (Move Cima)
Jogo (Jogador (1,0)) (Mapa 4[(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])])


==Exemplos de utilização para os diferentes movimentos

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) Parado
Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Cima)
Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,0)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Baixo)
Jogo (Jogador (2,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move esquerda)
Jogo (Jogador (1,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Direita)
Jogo (Jogador (3,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])


-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m  = Jogo (Jogador (px,py)) (Mapa lar (mapamove ((te,obs):tf) x y m 0))
                                                                                     where px=posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m
                                                                                           py=posy 0 (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m 
















                


