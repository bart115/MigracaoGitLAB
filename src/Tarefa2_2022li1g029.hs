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


group'::Eq a => [a] -> [[a]]
group'[]=[]
group'[x]=[[x]]
group' (h:t) |elem h (head r) = (h: (head r)):tail r 
             |otherwise = [h]: r 
                  where r=group' t


obslistrandom :: Int ->Int-> (Terreno, [Obstaculo]) -> [Obstaculo]
obslistrandom lar seed (Rio v ,obs)|lar==length obs =obs
                                   |length obs == (lar-1) && length (head (group' (obslistrandom lar seed (Rio v ,obs)))) + length (last (group' (obslistrandom lar seed (Rio v ,obs)))) >=5= [Nenhum] 
                                   |length obs == (lar-1) && elem Tronco obs ==False = obs ++[Tronco]
                                   |otherwise =obslistrandom lar (seed-1) (Rio v,[((proximosObstaculosValidos lar (Rio v,obs)) !! mod seed (length (proximosObstaculosValidos lar (Rio v,obs)) ))]++obs)
obslistrandom lar seed (te,obs) |lar==length obs = obs 
                                            |length obs == (lar-1) && elem Nenhum obs == False = obs ++ [Nenhum] 
                                            |otherwise =obslistrandom lar (seed-1) (te,[((proximosObstaculosValidos lar (te,obs)) !! mod seed (length (proximosObstaculosValidos lar (te,obs)) ))]++obs)

proximosObstaculosValidos x (Rio v,[])=[Nenhum,Tronco]
proximosObstaculosValidos x (Estrada v,[]) = [Nenhum,Tronco]
proximosObstaculosValidos x (Relva,[])=[Nenhum,Arvore]
proximosObstaculosValidos x (Rio v ,(Tronco:Tronco:Tronco:Tronco:Tronco:t))=[Nenhum]
proximosObstaculosValidos x (Rio v,obs)=[Nenhum,Tronco]
proximosObstaculosValidos x (Estrada v,(Carro:Carro:Carro:t))=[Nenhum]
proximosObstaculosValidos x (Estrada v,obs)=[Nenhum,Carro]
proximosObstaculosValidos x (Relva,obs) =[Nenhum,Arvore] 




--função principal que através das funções auxiliares estende o mapa

estendeMapa::Mapa ->Int->Mapa
estendeMapa (Mapa lar filas) seed = Mapa lar (filas ++ [(ta ,obslistrandom lar seed ((ta,[])))])
                                                                     where ta=terrenoaleatorio (Mapa lar filas)  seed 

