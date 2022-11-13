{- |
Module      : Tarefa1_2022li1g029
Description : Validação de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g029 where

import LI12223

{-|A função mapaValido vai receber um tipo 'Mapa' e vai dar um 'Booliano', esta função vai dizer
se o mapa recebido é um mapa em que se possa jogar ou se é um mapa que não pode existir.Esta função também esta 
definida por outras funções auxiliares como 'mapaValido1' , 'mapaValido2', 'mapaValido3, 'mapaValido4 , 'mapaValido5 , 'mapaValido6 , 'mapaValido7'
cada uma desta têm funções especificas que serão explicadas mais tarde.

==Exemplo de Utilização:
 >>> mapaValido 
 >>>-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa lr l) = (mapaValido1 (Mapa lr l) && mapaValido2 (Mapa lr l)) && (mapaValido3 (Mapa lr l) && mapaValido4 (Mapa lr l)) && (mapaValido5 (Mapa lr l) && mapaValido6 (Mapa lr l)) && mapaValido7 (Mapa lr l)

mapaValido1 :: Mapa -> Bool
mapaValido1 (Mapa lr []) = True
mapaValido1 (Mapa lr ((Relva,lo):t)) 
        |(elem Carro lo || elem Tronco lo) = False  
        |otherwise = mapaValido1 (Mapa lr t)
 
mapaValido1 (Mapa lr (((Rio vr),lo):t))
        |(elem Carro lo || elem Arvore lo) = False 
        |otherwise = mapaValido1 (Mapa lr t)
        
mapaValido1 (Mapa lr (((Estrada ve),lo):t))
        |(elem Arvore lo || elem Tronco lo) = False
        |otherwise = mapaValido1 (Mapa lr t)

--2--
mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa lr []) = True
mapaValido2 (Mapa lr (((Estrada ve),lo):t)) = mapaValido2 (Mapa lr t)
mapaValido2 (Mapa lr ((Relva,lo):t)) = mapaValido2 (Mapa lr t)

--se o proximo valor não for rio
mapaValido2 (Mapa lr (((Rio vr), lo) : ((Estrada ve),lo1) : t )) = mapaValido2 (Mapa lr t)
mapaValido2 (Mapa lr (((Rio vr), lo) : (Relva,lo1) :t)) = mapaValido2 (Mapa lr t)

--
mapaValido2 (Mapa lr [a]) = True  
mapaValido2 (Mapa lr (((Rio vr),lo) : ((Rio vr1),lo1) : t))
                |(vr > 0 && vr1 > 0) || (vr < 0 && vr1 < 0) = False
                |otherwise = mapaValido2 (Mapa lr (((Rio vr1),lo1):t))
--3--
mapaValido3 :: Mapa -> Bool
mapaValido3 (Mapa lr (((Estrada ve),lo):t)) = mapaValido3 (Mapa lr t)
mapaValido3 (Mapa lr ((Relva,lo):t)) = mapaValido3 (Mapa lr t)
mapaValido3 (Mapa lr []) = True
mapaValido3 (Mapa lr (((Rio vr),[]):t)) = mapaValido3 (Mapa lr t)
mapaValido3 (Mapa lr (((Rio vr),(o1:ot)):t)) 
                               |(o1 == Nenhum) && (length (head (aux3 (dropWhile (== Nenhum) ot))) > 5) = False
                               |(o1 == Tronco) && ((last ot) == Tronco) && (((length (head (aux3 (o1:ot)))) + (length (last (aux3 (o1:ot))))) > 5) = False  
                               |(o1 == Tronco) && ((length (head (aux3 (o1:ot)))) > 5) = False
                               |otherwise = mapaValido3 (Mapa lr t)

aux3 :: Eq a => [a] -> [[a]]
aux3 [] = []
aux3 (h:t) = (h:takeWhile (== h) t) : aux3 (dropWhile (== h) t)



--mapaValido3 (Mapa 4 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]) usar para testar, pois nao funciona
--usar também lista com mais de um elemento 

 
--4--
mapaValido4 :: Mapa -> Bool
mapaValido4 (Mapa lr (((Rio vr),lo):t)) = mapaValido4 (Mapa lr t)
mapaValido4 (Mapa lr ((Relva,lo):t)) = mapaValido4 (Mapa lr t)
mapaValido4 (Mapa lr []) = True
mapaValido4 (Mapa lr (((Estrada ve),[]):t)) = mapaValido4 (Mapa lr t)
mapaValido4 (Mapa lr (((Estrada ve),(o1:ot)):t)) 
                               |(o1 == Nenhum) && (length (head (aux4 (dropWhile (== Nenhum) ot))) > 3) = False
                               |(o1 == Carro) && ((last ot) == Carro) && (((length (head (aux4 (o1:ot)))) + (length (last (aux4 (o1:ot))))) > 3) = False  
                               |(o1 == Carro) && ((length (head (aux4 (o1:ot)))) > 3) = False
                               |otherwise = mapaValido3 (Mapa lr t)

aux4 :: Eq a => [a] -> [[a]]
aux4 [] = []
aux4 (h:t) = (h:takeWhile (== h) t) : aux4 (dropWhile (== h) t)

--5--
mapaValido5 :: Mapa -> Bool
mapaValido5 (Mapa lr []) = True 
mapaValido5 (Mapa lr ((Relva,lo):t))
                       |(elem Nenhum lo) == True = mapaValido5 (Mapa lr t) 
                       |otherwise = False

mapaValido5 (Mapa lr ((Rio vr,lo):t))
                       |(elem Nenhum lo) == True = mapaValido5 (Mapa lr t) 
                       |otherwise = False   

mapaValido5 (Mapa lr ((Estrada ve,lo):t))
                       |(elem Nenhum lo) == True = mapaValido5 (Mapa lr t) 
                       |otherwise = False

--6--
-- dá true com o 6-- mapaValido (Mapa 3 [(Estrada 2, [Carro,Carro,Nenhum]),(Relva,[Arvore,Nenhum,Nenhum]),(Rio 1 ,[Tronco,Tronco,Nenhum]),(Rio (-1),[Tronco,Tronco,Nenhum])])
mapaValido6 :: Mapa -> Bool
mapaValido6 (Mapa lr []) = True 
mapaValido6 (Mapa lr ((Relva,lo):t)) 
                      |lr /= length lo = False
                      |otherwise = mapaValido6 (Mapa lr t)

mapaValido6 (Mapa lr (((Rio vr),lo):t)) 
                      |lr /= length lo = False
                      |otherwise = mapaValido6 (Mapa lr t)

mapaValido6 (Mapa lr (((Estrada ve),lo):t)) 
                      |lr /= length lo = False
                      |otherwise = mapaValido6 (Mapa lr t)

--7--
mapaValido7 :: Mapa -> Bool
mapaValido7 (Mapa lr []) = True 
mapaValido7 (Mapa lr ((Relva,lo1):(Relva,lo2):(Relva,lo3):(Relva,lo4):(Relva,lo5):(Relva,lo6):t)) = False
mapaValido7 (Mapa lr (((Rio vr),lo):((Rio vr1),lo1):((Rio vr2),lo2):((Rio vr3),lo3):((Rio vr4),lo4):t)) = False
mapaValido7 (Mapa lr (((Estrada ve),lo):((Estrada ve1),lo1):((Estrada ve2),lo2):((Estrada ve3),lo3):((Estrada ve4),lo4):((Estrada ve5),lo5):t)) = False
mapaValido7 (Mapa lr ((tr,lo):t)) = mapaValido7 (Mapa lr t)
          
                                     