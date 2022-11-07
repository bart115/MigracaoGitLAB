{- |
Module      : Tarefa2_2022li1g029
Description : Geração contínua de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g029 where

import LI12223
--estendeMapa :: Mapa -> Int -> Mapa
--estendeMapa (Mapa x [h:t]) n = (Mapa  )
--            where 
--                element = (opçoes)!! mod n (length opçoes)
--                opçoes = proximosTerrenosValidos l




--função auxiliar
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]                                                              --quando não tem nenhum terreno
proximosTerrenosValidos (Mapa _ [(Rio _, _),(Rio _, _),(Rio _, _),(Rio _, _),_]) = [Estrada 0, Relva]                        --quando tem 4 rios seguidos 
proximosTerrenosValidos (Mapa _ [(Rio _,_),_]) = [Rio 0,Estrada 0,Relva]                                                     --quando tem um rio 
proximosTerrenosValidos (Mapa _ [(Estrada _,_),(Estrada _ ,_),(Estrada _,_),(Estrada _,_),(Estrada _, _),_ ])=[Rio 0, Relva] --quando tem 5 estradas seguidas
proximosTerrenosValidos (Mapa _ [(Estrada _,_),_]) = [Rio 0,Estrada 0,Relva]                                                 --quando tem uma estrada
proximosTerrenosValidos (Mapa _ [(Relva ,_),(Relva ,_),(Relva ,_),(Relva ,_),(Relva ,_)])=[Estrada 0 , Rio 0]                --quando tem 5 relvas seguidas
proximosTerrenosValidos (Mapa _ [(Relva,_),_]) = [Rio 0,Estrada 0,Relva]                                                     --quando tem uma relva



--função auxiliar
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos x (te,obs) |x==length obs = []
                                     |(x-1)==length obs && elem Nenhum obs =[Nenhum]
                                     |otherwise =proximosObstaculosValidosaux x (te,obs)
                where 
                    proximosObstaculosValidosaux x (Rio v,[])=[Nenhum,Tronco]
                    proximosObstaculosValidosaux x (Estrada v,[]) = [Nenhum,Tronco]
                    proximosObstaculosValidosaux x (Relva,[])=[Nenhum,Arvore]
                    proximosObstaculosValidosaux x (Rio v ,(Tronco:Tronco:Tronco:Tronco:Tronco:t))=[Nenhum]
                    proximosObstaculosValidosaux x (Rio v,obs)=[Nenhum,Tronco]
                    proximosObstaculosValidosaux x (Estrada v,(Carro:Carro:Carro:t))=[Nenhum]
                    proximosObstaculosValidosaux x (Estrada v,obs)=[Nenhum,Carro]
                    proximosObstaculosValidosaux x (Relva,obs) =[Nenhum,Arvore] 

