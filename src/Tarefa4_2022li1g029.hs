{- |
Module      : Tarefa4_2022li1g029
Description : Determinar se o jogo terminou
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g029 where

import LI12223

jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa lr l)) = jogoTerminou1 (Jogo (Jogador (x,y)) (Mapa lr l))
--MORREU PELA DIREITA
jogoTerminou1 :: Jogo -> Bool 
jogoTerminou1 (Jogo (Jogador (x,y)) (Mapa lr l)) = x > lr || x < 0 || y > length l
                                
jogoTerminou2 :: Jogo -> Bool
jogoTerminou2 (Jogo (Jogador (x,y)) (Mapa lr [])) = False 
jogoTerminou2 (Jogo (Jogador (x,y)) (Mapa lr ((tr,lo):t))) |y==0 && hanenhum x lo = True 
                                                           |otherwise = jogoTerminou2 (Jogo (Jogador (x,y-1)) (Mapa lr t)))

hanenhum::Int->[Obstaculo]->Bool
hanenhum x [] =False
hanenhum x (o1:ot) |x==0 && o1==Nenhum=True
                   |otherwise = hanenhum (x-1) ot




--carro se as coordenadas do carro estiverem no mesmo lugar que o boneco
--rio se estiver no mesmo siteo que o rio -- para isso tenho de ver na lista de obstaculos 

--type Coordenadas = (Int, Int)
--data Jogador = Jogador Coordenadas
--data Jogo = Jogo Jogador Mapa