{- |
Module      : Tarefa2_2022li1g029
Description : Geração contínua de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g029 where

import LI12223
import Data.List.NonEmpty (groupAllWith1)

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa lar [(Estrada v1 ,obs1),(Estrada v2 ,obs2),(Estrada v3 ,obs3),(Estrada v4 ,obs4),(Estrada v5 ,obs5)])=[Rio 0,Relva]
proximosTerrenosValidos (Mapa lar (h:[(Estrada  v1,obs1),(Estrada  v2, obs2),(Estrada v3, obs3),(Estrada v4, obs4),(Estrada v5, obs5)]))=[Rio 0, Relva]
proximosTerrenosValidos (Mapa lar [])=[Relva,Rio 0 ,Estrada 0]
proximosTerrenosValidos (Mapa lar [(Rio v1, obs1),(Rio v2, obs2),(Rio v3, obs3),(Rio v4, obs4)]) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa lar (h:[(Rio v1, obs1),(Rio v2, obs2),(Rio v3, obs3),(Rio v4, obs4)])) = [Estrada 0, Relva]
proximosTerrenosValidos (Mapa lar [(Relva ,obs1),(Relva, obs2),(Relva , obs3),(Relva , obs4),(Relva , obs5)])=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa lar (h:[(Relva ,obs1),(Relva, obs2),(Relva , obs3),(Relva , obs4),(Relva , obs5)]))=[Estrada 0 , Rio 0]
proximosTerrenosValidos (Mapa lar t)=[Relva,Rio 0 ,Estrada 0]


proximosObstaculosValidos :: Int-> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos lar (Rio v,y)|length y==lar =[]
                                       |length y ==(lar-1) && elem Tronco y == False = [Tronco]
                                       |otherwise = proximosObstaculosValidosAux lar (Rio v,y) 
proximosObstaculosValidos lar (x,y) |length y== lar = [] 
                                    |length y == (lar-1) && elem Nenhum y == False = [Nenhum]
                                    |otherwise = proximosObstaculosValidosAux lar (x,y)


proximosObstaculosValidosAux::Int-> (Terreno,[Obstaculo])->[Obstaculo]
proximosObstaculosValidosAux a (Rio b, [])=[Nenhum, Tronco]
proximosObstaculosValidosAux a (Estrada b,[])=[Nenhum, Carro]
proximosObstaculosValidosAux a (Relva, [])=[Nenhum, Arvore]
proximosObstaculosValidosAux a (Rio b,l)=[Nenhum,Tronco]
proximosObstaculosValidosAux a (Estrada b,l)=[Nenhum,Carro]
proximosObstaculosValidosAux a (Relva , l)=[Nenhum,Arvore]



randommap::Mapa->Int->Terreno
randommap (Mapa lar l) seed = proximosTerrenosValidos (Mapa lar l) !! mod seed (length (proximosTerrenosValidos (Mapa lar l)))


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



   
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa lar l) seed = Mapa lar (l ++ [(randommap (Mapa lar l) seed ,randomobs lar seed (proximosObstaculosValidos lar (randommap (Mapa lar l) seed , [])))])
