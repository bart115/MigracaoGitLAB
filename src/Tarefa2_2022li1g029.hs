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

group'::Eq a => [a] -> [[a]]
group'[]=[]
group'[x]=[[x]]
group' (h:t) |elem h (head r) = (h: (head r)):tail r 
             |otherwise = [h]: r 
                  where r = group' t



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
proximosObstaculosValidos lar (x,y) |length y== lar = [] 
                                    |length y == (lar-1) && elem Nenhum y == False = [Nenhum]
                                    |otherwise = proximosObstaculosValidosAux lar (x,y)

{-}
proximosObstaculosValidos lar (Rio v ,obs)|lar==length obs =[]
                                          |head obs == Tronco && last obs == Tronco && length obs == (lar-1) && length (head (group' (proximosObstaculosValidos lar (Rio v ,obs)))) + length (last (group' (proximosObstaculosValidos lar  (Rio v ,obs)))) >=5= [Nenhum] 
                                          |length obs == (lar-1) && elem Tronco obs ==False = [Tronco]
                                          |otherwise = proximosObstaculosValidosAux lar (Rio v,obs)
proximosObstaculosValidos lar  (Relva,obs) |lar==length obs = []
                                           |length obs == (lar-1) && elem Nenhum obs == False = obs ++ [Nenhum] 
                                           |otherwise = proximosObstaculosValidosAux lar  (Relva,obs) 
proximosObstaculosValidos lar  (Estrada v,obs) |lar==length obs = []
                                       |head obs == Carro && last obs == Carro && length obs == (lar-1) && length (head (group' (proximosObstaculosValidos lar  (Estrada v ,obs)))) + length (last (group' (proximosObstaculosValidos lar  (Estrada v ,obs)))) >=5= [Nenhum] 
                                       |length obs == (lar-1) && elem Nenhum obs == False = obs ++ [Nenhum] 
                                       |otherwise =proximosObstaculosValidosAux lar  (Estrada v,obs)

-}



proximosObstaculosValidosAux::Int-> (Terreno,[Obstaculo])->[Obstaculo]
proximosObstaculosValidosAux a (Rio b, [])=[Nenhum, Tronco]
proximosObstaculosValidosAux a (Estrada b,[])=[Nenhum, Carro]
proximosObstaculosValidosAux a (Relva, [])=[Nenhum, Arvore]
proximosObstaculosValidosAux a (Rio b,t)|last t ==Tronco && last (init t) ==Tronco && last (init (init t)) ==Tronco && last (init (init (init t))) ==Tronco&&last (init (init (init (init t)))) ==Tronco =[Nenhum]
proximosObstaculosValidosAux a (Rio b,l)=[Nenhum,Tronco]
proximosObstaculosValidosAux a (Estrada b,t:[Carro,Carro,Carro])=[Nenhum]
proximosObstaculosValidosAux a (Estrada b,l)=[Nenhum,Carro]
proximosObstaculosValidosAux a (Relva , l)=[Nenhum,Arvore]





randommap::Mapa->Int->Terreno
randommap (Mapa lar l) seed = proximosTerrenosValidos (Mapa lar l) !! mod seed (length (proximosTerrenosValidos (Mapa lar l)))


randomobs::Int->Int->[Obstaculo]->[Obstaculo]
randomobs lar seed [] = []
randomobs lar seed filas |lar==length filas = filas
                     |(lar-1)==length filas && elem Nenhum filas == False =filas++[Nenhum]
                     |otherwise = randomobs lar (seed+3) (filas++[filas !! mod (seed+1) (length filas)])

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa lar l) seed = Mapa lar (l ++ [(randommap (Mapa lar l) seed ,randomobs lar seed (proximosObstaculosValidos lar (randommap (Mapa lar l) seed , [])))])
