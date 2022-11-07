{- |
Module      : Tarefa1_2022li1g029
Description : Validação de um mapa
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g029 where

import LI12223

--1--
mapaValido :: Mapa -> Bool
mapaValido (Mapa lr l) = (mapaValido1 (Mapa lr l) && mapaValido2 (Mapa lr l)) && (mapaValido3 (Mapa lr l) && mapaValido4 (Mapa lr l)) && (mapaValido5 (Mapa lr l) && mapaValido6 (Mapa lr l))

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
--No caso o caso de paragem é quando tem um elemento pois este já nao compara com mais nenhum
mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa lr []) = True
mapaValido2 (Mapa lr (((Estrada ve),lo):t)) = mapaValido2 (Mapa lr t)
mapaValido2 (Mapa lr ((Relva,lo):t)) = mapaValido2 (Mapa lr t)
--
mapaValido2 (Mapa lr [(a,b)]) = True  
mapaValido2 (Mapa lr (((Rio vr),lo) : ((Rio vr1),lo1) : t))
                |(vr > 0 && vr1 > 0) || (vr < 0 && vr1 < 0) = False
                |otherwise = mapaValido2 (Mapa lr (((Rio vr1),lo1):t))
--3--
mapaValido3 :: Mapa -> Bool 
mapaValido3 (Mapa lr []) = True 
mapaValido3 (Mapa lr (((Estrada ve),(o1:ot)): t)) = mapaValido3 (Mapa lr t)
mapaValido3 (Mapa lr ((Relva,(o1:ot)): t)) = mapaValido3 (Mapa lr t)
mapaValido3 (Mapa lr (((Rio vr),(o1:ot)): t)) 
             |(aux (Mapa lr (((Rio vr),(o1:ot)):t)) 0) > 5 = False 
             |otherwise = mapaValido3 (Mapa lr t) 


aux :: Mapa -> Int -> Int 
aux (Mapa lr (((Rio vr),[]):t)) n = n
aux (Mapa lr (((Rio vr),(o1:ot)):t)) n 
                      | o1 == Tronco = aux (Mapa lr (((Rio vr),ot):t)) (n+1)
                      |otherwise = aux (Mapa lr (((Rio vr),ot):t)) n

--4--
mapaValido4 :: Mapa -> Bool 
mapaValido4 (Mapa lr []) = True 
mapaValido4 (Mapa lr (((Rio vr),(o1:ot)): t)) = mapaValido4 (Mapa lr t)

mapaValido4 (Mapa lr ((Relva,(o1:ot)): t)) = mapaValido4 (Mapa lr t)

mapaValido4 (Mapa lr (((Estrada ve),(o1:ot)): t)) 
             |(aux1 (Mapa lr (((Estrada ve),(o1:ot)):t)) 0) > 3 = False 
             |otherwise = mapaValido4 (Mapa lr t) 


aux1 :: Mapa -> Int -> Int 
aux1 (Mapa lr (((Estrada ve),[]):t)) n = n
aux1 (Mapa lr (((Estrada ve),(o1:ot)):t)) n 
                      | o1 == Carro = aux1 (Mapa lr (((Estrada ve),ot):t)) (n+1)
                      |otherwise = aux1 (Mapa lr (((Estrada ve),ot):t)) n


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
--mapaValido7 (Mapa lr (((tr),lo):t)) 


mapaValido7 (Mapa lr (((Rio vr),lo):t))
            |(aux2 (Mapa lr (((Rio vr),lo):t)) 0) > 4 = False
            |otherwise = True 



aux2 :: Mapa -> Int -> Int
aux2 (Mapa lr [(a,b)]) n = n
aux2 (Mapa lr (((Rio vr),lo): ((Rio vr1),lo1): t)) n
               |(Rio vr) == (Rio vr1) = aux2 (Mapa lr (((Rio vr1),lo1):t)) (n+1)
               |otherwise = aux2 (Mapa lr (((Rio vr1),lo1):t)) n
                                        --aqui não pode pedir RIO tem de pedir as outras 
