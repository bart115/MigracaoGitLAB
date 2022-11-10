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
A função posx calcula a posição em x após a jogada e está definida por: 

@
posx::Jogo->Jogada->Int
posx (Jogo (Jogador (x,y)) (Mapa lar [])) Parado = x
posx (Jogo (Jogador (x,y)) (Mapa lar ((Rio v,obs):tf))) Parado |y==0 && hatronco x obs = x+v
                                                               |otherwise = posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) Parado = posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Esquerda) =x-1
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Direita)= x+1
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m =x
@
-}

posx::Jogo->Jogada->Int
posx (Jogo (Jogador (x,y)) (Mapa lar [])) Parado = x
posx (Jogo (Jogador (x,y)) (Mapa lar ((Rio v,obs):tf))) Parado |y==0 && hatronco x obs = x+v
                                                               |otherwise = posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) Parado = posx (Jogo (Jogador (x,y-1)) (Mapa lar tf)) Parado
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Esquerda) =x-1
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Direita)= x+1
posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m =x



{-|A função posy calcula a posição em y após a jogada e está definida por:

@
posy::Jogo->Jogada->Int 
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Baixo)  =(y-1)
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Cima) |numfilas (Mapa lar ((te,obs):tf)) > y =(y+1)
                                                                 |otherwise = y
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m = y
@  
-}

posy::Jogo->Jogada->Int 
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Baixo)  =(y-1)
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) (Move Cima) |numfilas (Mapa lar ((te,obs):tf)) > y =(y+1)
                                                                 |otherwise = y
posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m = y




{- |A proxima função auxiliar tem como função verificar se na posição do jogador se encontra um tronco

A função hatronco está definida por:

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

{- | Para dar movimento aos obstaculos fez-se as duas funções auxiliares 

A função obsmove move os obstaculo de uma "linha" do jogo e está definida por:

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
obsmove::(Terreno,[Obstaculo])->[Obstaculo]
obsmove (Relva,obs) = obs
obsmove (Rio v,obs)  |v==0 = obs
                     |v>0 = obsmove (Rio (v-1),(last obs: init obs ))
                     |otherwise= obsmove (Rio (v+1),tail obs ++ [head obs] )
obsmove (Estrada v,obs) |v==0 = obs
                        |v>0 = obsmove (Estrada (v-1),last obs: init obs )
                        |otherwise = obsmove (Estrada (v+1),tail obs ++ [head obs])

{- |A função mapamove usa a função obsmove para mover todos os obstaculos do mapa da seguinte forma: 

@
mapamove::[(Terreno,[Obstaculo])]->[(Terreno,[Obstaculo])]
mapamove [(te,obs)] = [(te,obsmove (te,obs))]
mapamove ((te,obs):fs) = (te,obsmove (te,obs)):mapamove fs
@
-}
mapamove::[(Terreno,[Obstaculo])]->[(Terreno,[Obstaculo])]
mapamove [(te,obs)] = [(te,obsmove (te,obs))]
mapamove ((te,obs):fs) = (te,obsmove (te,obs)):mapamove fs
               



{-|Por fim , a função numfilas conta o numero de filas do mapa para impedir o jogador de avançar se este estiver no limite superior do mapa

A função numfilas está definida por:

@
numfilas::Mapa->Int
numfilas (Mapa l [])=0
numfilas (Mapa l (ob1:obs))=1+numfilas (Mapa l obs)
@
-}
numfilas::Mapa->Int
numfilas (Mapa l [])=0
numfilas (Mapa l (ob1:obs))=1+numfilas (Mapa l obs)



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

>>>animaJogo (Jogo (Jogador (1,2)) (Mapa 4[(Rio (-1),[Tronco,Nenhum,Nenhum,Tronco),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])])) (Move Cima)
Jogo (Jogador (1,2)) (Mapa 4[(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Arvore])])


==Exemplos de utilização para os diferentes movimentos

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) Parado
Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Cima)
Jogo (Jogador (2,3)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Baixo)
Jogo (Jogador (2,1)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move esquerda)
Jogo (Jogador (1,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])

>>>animaJogo (Jogo (Jogador (2,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Nenhum,Tronco,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])) (Move Direita)
Jogo (Jogador (3,2)) (Mapa 3 [(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco]),(Relva,[Nenhum,Arvore,Nenhum])])


-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m  = Jogo (Jogador (px,py)) (Mapa lar (mapamove ((te,obs):tf)))
                                                                                     where px=posx (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m
                                                                                           py=posy (Jogo (Jogador (x,y)) (Mapa lar ((te,obs):tf))) m 
















                


