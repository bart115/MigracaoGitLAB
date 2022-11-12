{- |
Module      : Tarefa2_2022li1g029
Description : Geração contínua de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g029 where

import LI12223


{-|Na realização desta tarefa usei algumas funções auxiliares para chegar ao objetivo final
A função 'proximosTerrenosValidos' dá-nos a lista de terrenos válidos para a próxima "linha" do mapa e está definida por:

@
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa lar [])=[Relva,Rio 0 ,Estrada 0]
proximosTerrenosValidos (Mapa lar ((Estrada  v1,obs1):(Estrada  v2, obs2):(Estrada v3, obs3):(Estrada v4, obs4):(Estrada v5, obs5):tf)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa lar ((Rio v1, obs1):(Rio v2, obs2):(Rio v3, obs3):(Rio v4, obs4):tf)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa lar ((Relva ,obs1):(Relva, obs2):(Relva , obs3):(Relva , obs4):(Relva , obs5):tf)) =[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa lar t)=[Relva,Rio 0 ,Estrada 0]
@
-}

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa lar [])=[Relva,Rio 0 ,Estrada 0]
proximosTerrenosValidos (Mapa lar ((Estrada  v1,obs1):(Estrada  v2, obs2):(Estrada v3, obs3):(Estrada v4, obs4):(Estrada v5, obs5):tf)) = [Rio 0, Relva]
proximosTerrenosValidos (Mapa lar ((Rio v1, obs1):(Rio v2, obs2):(Rio v3, obs3):(Rio v4, obs4):tf)) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa lar ((Relva ,obs1):(Relva, obs2):(Relva , obs3):(Relva , obs4):(Relva , obs5):tf)) =[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa lar t)=[Relva,Rio 0 ,Estrada 0]



{-|A função 'proximosObstaculosValidosAux' dá a lista de obstáculos disponiveis para serem usados numa posição para casos mais gerais e serve de auxiliar á função proximosObstaculosValidos
A função 'proximosObstaculosValidosAux está definida por:  

@
proximosObstaculosValidosAux::Int-> (Terreno,[Obstaculo])->[Obstaculo]
proximosObstaculosValidosAux a (Rio b, [])=[Nenhum, Tronco]
proximosObstaculosValidosAux a (Estrada b,[])=[Nenhum, Carro]
proximosObstaculosValidosAux a (Relva, [])=[Nenhum, Arvore]
proximosObstaculosValidosAux a (Rio b,l)=[Nenhum,Tronco]
proximosObstaculosValidosAux a (Estrada b,l)=[Nenhum,Carro]
proximosObstaculosValidosAux a (Relva , l)=[Nenhum,Arvore]
@
-}

proximosObstaculosValidosAux::Int-> (Terreno,[Obstaculo])->[Obstaculo]
proximosObstaculosValidosAux a (Rio b, [])=[Nenhum, Tronco]
proximosObstaculosValidosAux a (Estrada b,[])=[Nenhum, Carro]
proximosObstaculosValidosAux a (Relva, [])=[Nenhum, Arvore]
proximosObstaculosValidosAux a (Rio b,l)=[Nenhum,Tronco]
proximosObstaculosValidosAux a (Estrada b,l)=[Nenhum,Carro]
proximosObstaculosValidosAux a (Relva , l)=[Nenhum,Arvore]


{-|A função 'proximosObstaculosValidos' dá a lista de obstaculos disponiveis para serem usados numa posição e está definida por:

@
proximosObstaculosValidos :: Int-> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos lar (Rio v,y)|length y==lar =[]
                                       |length y ==(lar-1) && elem Tronco y == False = [Tronco]
                                       |otherwise = proximosObstaculosValidosAux lar (Rio v,y) 
proximosObstaculosValidos lar (x,y) |length y== lar = [] 
                                    |length y == (lar-1) && elem Nenhum y == False = [Nenhum]
                                    |otherwise = proximosObstaculosValidosAux lar (x,y)
@
-}

proximosObstaculosValidos :: Int-> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos lar (Rio v,y)|length y==lar =[]
                                       |length y ==(lar-1) && elem Tronco y == False = [Tronco]
                                       |otherwise = proximosObstaculosValidosAux lar (Rio v,y) 
proximosObstaculosValidos lar (x,y) |length y== lar = [] 
                                    |length y == (lar-1) && elem Nenhum y == False = [Nenhum]
                                    |otherwise = proximosObstaculosValidosAux lar (x,y)


{-|A função randommap dá um terreno aleatório para a próxima "linha" do mapa
A função randommap está definida por:

@
randommap::Mapa->Int->Terreno
randommap (Mapa lar l) seed = proximosTerrenosValidos (Mapa lar l) !! mod seed (length (proximosTerrenosValidos (Mapa lar l)))
@
-}

randommap::Mapa->Int->Terreno
randommap (Mapa lar t) seed = proximosTerrenosValidos (Mapa lar t) !! mod seed (length (proximosTerrenosValidos (Mapa lar t)))




{-|A função 'randomobs' dá a lista de obstáculos aleatórios do proximo nivel tendo em conta o terreno aleatório da proxima "linha".
A função randomobs apesar de ser aleatório está preparada para

*Não gerar 6 troncos seguidos
*Nao gerar 3 carros seguidos 

@
randomobs::Int->Int->[Obstaculo]->[Obstaculo]
randomobs lar seed [] = []
randomobs lar seed obs |lar==length obs = obs
                       |last obs ==Tronco && last (init obs) ==Tronco && last (init (init obs)) ==Tronco && last (init (init (init obs))) ==Tronco&&last (init (init (init (init obs)))) ==Tronco = randomobs lar seed (obs++[Nenhum])
                       |last obs ==Carro && last (init obs)==Carro && last (init(init obs)) ==Carro = randomobs lar seed (obs++[Nenhum])
                       |(lar-1)==length obs && head obs ==Tronco && last obs == Tronco && length (head (group' obs)) + length (last (group' obs))>5=randomobs lar seed (obs++[Nenhum]) 
                       |(lar-1)==length obs && head obs ==Carro && last obs == Carro && length (head (group' obs)) + length (last (group' obs))>3=randomobs lar seed (obs++[Nenhum]) 
                       |otherwise = randomobs lar (seed+3) (obs++[obs !! mod (seed+1) (length obs)])
                                                    where 
                                                          group'::Eq a => [a] -> [[a]]
                                                          group'[]=[]
                                                          group'[x]=[[x]]
                                                          group' (h:t) |elem h (head r) = (h: (head r)):tail r 
                                                                       |otherwise = [h]: r 
                                                                               where r = group' t
@
-}

randomobs::Int->Int->[Obstaculo]->[Obstaculo]
randomobs lar seed [] = []
randomobs lar seed obs |lar==length obs = obs
                       |last obs ==Tronco && last (init obs) ==Tronco && last (init (init obs)) ==Tronco && last (init (init (init obs))) ==Tronco&&last (init (init (init (init obs)))) ==Tronco = randomobs lar seed (obs++[Nenhum])
                       |last obs ==Carro && last (init obs)==Carro && last (init(init obs)) ==Carro = randomobs lar seed (obs++[Nenhum])
                       |(lar-1)==length obs && head obs ==Tronco && last obs == Tronco && length (head (group' obs)) + length (last (group' obs))>5=randomobs lar seed (obs++[Nenhum]) 
                       |(lar-1)==length obs && head obs ==Carro && last obs == Carro && length (head (group' obs)) + length (last (group' obs))>3=randomobs lar seed (obs++[Nenhum]) 
                       |otherwise = randomobs lar (seed+3) (obs++[obs !! mod (seed+1) (length obs)])
                                                    where 
                                                          group'::Eq a => [a] -> [[a]]
                                                          group'[]=[]
                                                          group'[x]=[[x]]
                                                          group' (h:t) |elem h (head r) = (h: (head r)):tail r 
                                                                       |otherwise = [h]: r 
                                                                               where r = group' t



{-|Finalmente,a função ´estendeMapa´ adiciona uma "linha" ao mapa com um terreno e obstaculos aleatorios.

==Exemplos de utilização 

>>>estendeMapa (Mapa 4 [(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum])]) 16
Mapa 4 [(Estrada 0,[Nenhum,Carro,Carro,Carro]),(Relva,[Arvore,Nenhum,Arvore,Nenhum]),(Relva,[Nenhum,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum,Nenhum]),(Relva,[Arvore,Arvore,Nenhum,Nenhum])] 

>>>estendeMapa (Mapa 7 [(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum])]) 25 
Mapa 7 [(Relva,[Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum]),(Rio 1,[Tronco,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum])]


>>>estendeMapa (Mapa 6 [(Estrada 1,[Carro,Carro,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum,Carro,Carro]),(Estrada 1,[Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro])]) 3
Mapa 6 [(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore]),(Estrada 1,[Carro,Carro,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Carro,Nenhum,Carro,Carro]),(Estrada 1,[Carro,Nenhum,Nenhum,Nenhum,Carro,Carro]),(Estrada (-1),[Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro]),(Estrada 1,[Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro])]



A função estendeMapa está definida por: 

@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa lar t) seed = Mapa lar ([(randommap (Mapa lar t) seed ,randomobs lar seed (proximosObstaculosValidos lar (randommap (Mapa lar t) seed , [])))] ++ t)
@
-}

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa lar t) seed = Mapa lar ([(randommap (Mapa lar t) seed ,randomobs lar seed (proximosObstaculosValidos lar (randommap (Mapa lar t) seed , [])))] ++ t)
