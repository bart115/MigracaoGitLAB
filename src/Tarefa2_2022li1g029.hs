{- |
Module      : Tarefa2_2022li1g029
Description : Geração contínua de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g029 where

import LI12223


terrenoaleatorio::[Terreno]->Int->Terreno
terrenoaleatorio l n= (proximosTerrenosValidos (Mapa x l) ) !! mod n (length (proximosTerrenosValidos (Mapa x l)))

                      --  where opçoes1 = proximosTerrenosValidos (Mapa x l)

obstaculoalatorio::[Obstaculo]->Int->Obstaculo
obstaculoaleatorio l n= (proximosObstaculosValidos x) !! mod n (length (proximosObstaculosValidos x ))

                      --  where opçoes2 = proximosObstaculosValidos x

estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa x l) n | x==1 = Mapa x (l++ [(terrenoaleatorio (proximosTerrenosValidos (Mapa x l) n ),[obstaculoaleatorio (proximoObstaculosValidos x (terrenoaleatorio (proximosTerrenosValidos (Mapa x l) n )) n])])
                         | otherwise = estendeMapa Mapa (x-1) (l:(terrenoaleatorio,[obstaculoaleatorio]++[obstaculoaleatorio]))




--função auxiliar que dá os terrenos válidos
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ []) = [Rio 0, Estrada 0, Relva]                                                              --quando não tem nenhum terreno
proximosTerrenosValidos (Mapa _ [(Rio _, _),(Rio _, _),(Rio _, _),(Rio _, _),_]) = [Estrada 0, Relva]                        --quando tem 4 rios seguidos 
proximosTerrenosValidos (Mapa _ [(Rio _,_),_]) = [Rio 0,Estrada 0,Relva]                                                     --quando tem um rio 
proximosTerrenosValidos (Mapa _ [(Estrada _,_),(Estrada _ ,_),(Estrada _,_),(Estrada _,_),(Estrada _, _),_ ])=[Rio 0, Relva] --quando tem 5 estradas seguidas
proximosTerrenosValidos (Mapa _ [(Estrada _,_),_]) = [Rio 0,Estrada 0,Relva]                                                 --quando tem uma estrada
proximosTerrenosValidos (Mapa _ [(Relva ,_),(Relva ,_),(Relva ,_),(Relva ,_),(Relva ,_)])=[Estrada 0 , Rio 0]                --quando tem 5 relvas seguidas
proximosTerrenosValidos (Mapa _ [(Relva,_),_]) = [Rio 0,Estrada 0,Relva]                                                     --quando tem uma relva



--função auxiliar que dá os obstaculos válidos 
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos x (te,obs) |x==length obs = []
                                     |length obs == (x-1) && elem Nenhum obs == False =[Nenhum]
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

