{- |
Module      : Tarefa2_2022li1g029
Description : Geração contínua de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g029 where

import LI12223




--função auxiliar que dá os terrenos válidos para a proxima linha

proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa l []) = [Rio 0, Estrada 0, Relva]                                                                       --quando não tem nenhum terreno
proximosTerrenosValidos (Mapa l (t:[(Rio v1, ob1),(Rio v2, ob2),(Rio v3, ob3),(Rio v4, ob4)])) = [Estrada 0, Relva]                   --exceção: quando tem 4 rios seguidos 
proximosTerrenosValidos (Mapa l (t:[(Estrada v1,ob1),(Estrada _ ,_),(Estrada _,_),(Estrada _,_),(Estrada _, _)]) )=[Rio 0, Relva]     --exceção: quando tem 5 estradas seguidas
proximosTerrenosValidos (Mapa l (t:[(Relva ,_),(Relva ,_),(Relva ,_),(Relva ,_),(Relva ,_)]))=[Estrada 0 , Rio 0]                     --exceção: quando tem 5 relvas seguidas                                           
proximosTerrenosValidos (Mapa l t) = [Rio 0, Estrada 0, Relva]                                                                        --restantes casos


--função auxiliar que dá o terreno aleatório da proxima linha 

terrenoaleatorio::Mapa->Int->Terreno
terrenoaleatorio (Mapa x l) seed = proximosTerrenosValidos (Mapa x l) !! mod seed (length (proximosTerrenosValidos (Mapa x l)))   


--função auxiliar que dá os obstaculos válidos para uma linha com um terreno prédefinida 
--obs:alterei a função para que não desse apenas os obstaculos válidos mas para que desse logo uma lista de obstaculos aleatórios 

proximosObstaculosValidos :: Int ->Int-> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos lar seed (te,obs) |lar==length obs = obs 
                                            |length obs == (lar-1) && elem Nenhum obs == False = obs ++ [Nenhum] 
                                            |otherwise =proximosObstaculosValidos lar seed (te,[((proximosObstaculosValidosaux lar (te,obs)) !! mod seed (length (proximosObstaculosValidosaux lar (te,obs)) ))]++obs)
              where 
                    proximosObstaculosValidosaux x (Rio v,[])=[Nenhum,Tronco]
                    proximosObstaculosValidosaux x (Estrada v,[]) = [Nenhum,Tronco]
                    proximosObstaculosValidosaux x (Relva,[])=[Nenhum,Arvore]
                    proximosObstaculosValidosaux x (Rio v ,(Tronco:Tronco:Tronco:Tronco:Tronco:t))=[Nenhum]
                    proximosObstaculosValidosaux x (Rio v,obs)=[Nenhum,Tronco]
                    proximosObstaculosValidosaux x (Estrada v,(Carro:Carro:Carro:t))=[Nenhum]
                    proximosObstaculosValidosaux x (Estrada v,obs)=[Nenhum,Carro]
                    proximosObstaculosValidosaux x (Relva,obs) =[Nenhum,Arvore] 




--função principal que através das funções auxiliares estende o mapa

estendeMapa::Mapa ->Int->Mapa
estendeMapa (Mapa lar filas) seed = Mapa lar (filas ++ [(ta ,proximosObstaculosValidos lar seed ((ta,[])))])
                                                                     where ta=terrenoaleatorio (Mapa lar filas)  seed 

