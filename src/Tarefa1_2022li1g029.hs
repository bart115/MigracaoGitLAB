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
 >>> mapaValido (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 1,[Carro,Nenhum,Carro]),(Rio 1,[Tronco,Nenhum,Tronco])]) 
 >>> True 

 >>> mapaValido (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio 1,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Estrada 1,[Carro,Carro,Carro]),(Rio 1,[Tronco,Nenhum,Tronco])])]
 >>> False-}
mapaValido :: Mapa -> Bool
mapaValido (Mapa lr l) = (obsvalidos (Mapa lr l) && mapaValido2 (Mapa lr l)) && (mapaValido3 (Mapa lr l) && mapaValido4 (Mapa lr l)) && (mapaValido5 (Mapa lr l) && mapaValido6 (Mapa lr l)) && mapaValido7 (Mapa lr l) && arvores_maximas_juntas (Mapa lr l)

{-|Como referi anteriormentente a função principal, 'mapaValido' é composta por algumas auxiliares sendo uma delas a função 'mapaValido1',
esta função recebe um mapa e confirma se nesse mapa consoante o terreno os obstaculos estão corretos.

==Exemplo de Utilização:

 >>> mapaValido1 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Arvore])])
 >>> False 

 >>> mapaValido1 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco])])
 >>> True

 >>> mapaValido1 (Mapa 3 [(Relva,[Nenhum,Nenhum,Carro]),(Estrada 3,[Nenhum,Nenhum,Carro]),(Estrada 3,[Nenhum,Nenhum,Arvore])])
 >>> False

-}
obsvalidos :: Mapa -> Bool
obsvalidos (Mapa _ []) = True 
obsvalidos (Mapa l (x:xs)) | mapaValido1 x = obsvalidos (Mapa l  xs)
                       | otherwise = False

mapaValido1 :: (Terreno,[Obstaculo]) -> Bool
mapaValido1 (Relva,lo) = not $ elem Carro lo || elem Tronco lo 
mapaValido1 ((Rio vr),lo) = not $ elem Carro lo || elem Arvore lo
mapaValido1 ((Estrada ve),lo) = not $ elem Arvore lo || elem Tronco lo

{-|A função 'mapaValido2' recebe um @Mapa@ e compara os terrenos, se o terrenos forem @Estrada@ , @Relva@ , @Rio@ e depois outra qualquer 
a função faz a recursiva se os terrenos forem @Rio@ e depois outro @Rio@ a função compara as velocidades. Se ambas forem tiverem o mesmo sinal então ambos os rios estão a dirigir se 
para a mesma direção. Esta função pretende que os seguidos nunca estejam a ir pela mesma direção.

==Exemplos de Utilização:
 >>> mapaValido2 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Rio (-1),[Nenhum,Nenhum,Carro]),(Rio 1,[Nenhum,Nenhum,Carro])])
 >>> True

 >>> mapaValido2 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Carro]),(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Carro]),(Rio 1,[Nenhum,Nenhum,Carro])])
 >>> False-}
--2--
mapaValido2 :: Mapa -> Bool
mapaValido2 (Mapa lr []) = True
mapaValido2 (Mapa lr (((Estrada ve),lo):t)) = mapaValido2 (Mapa lr t)
mapaValido2 (Mapa lr ((Relva,lo):t)) = mapaValido2 (Mapa lr t)


mapaValido2 (Mapa lr (((Rio vr), lo) : ((Estrada ve),lo1) : t )) = mapaValido2 (Mapa lr t)
mapaValido2 (Mapa lr (((Rio vr), lo) : (Relva,lo1) :t)) = mapaValido2 (Mapa lr t)


mapaValido2 (Mapa lr [a]) = True  
mapaValido2 (Mapa lr (((Rio vr),lo) : ((Rio vr1),lo1) : t))
                |(vr > 0 && vr1 > 0) || (vr < 0 && vr1 < 0) = False
                |otherwise = mapaValido2 (Mapa lr (((Rio vr1),lo1):t))

{-|A função 'mapaValido3' tem como objetivo contar a quantidade de troncos que existe num rio e caso esse valor for maior que 
5 então a função diz que @Mapa@ não é valido , para isso é utilizado uma função auxiliar chamada 'aux3' que junta os elementos iguais da lista 
de obstaculos em listas, assim é só pedir o comprimento da lista se a cabeça da lista de obstaculos for Tronco.
Nesta função existe uma exceção que caso na lista de obstaculos o primeiro elemento for tronco e igual ao ultimo então, ter se á de somar o comprimento do primeiro elemento da lista 
com o comprimento do ultimo elemento da lista dos auxiliares.

==Exemplos de Utilização:
 >>> mapaValido3 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco])])
 >>> True

 >>> mapaValido3 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco])])
 >>> False-}

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

{-|Esta função 'aux3' simplesmente recebe a lista dos obstaculos e cria uma lista de listas com os elementos iguais dessa lista juntos
Utilizando o exemplo da função 'mapaValido3'.

==Exemplos de Utilização:
 
 >>> aux3 [Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco]
 >>> [[Tronco,Tronco,Tronco],[Nenhum,Nenhum],[Tronco,Tronco]]

 >>> aux3 [Tronco,Tronco,Tronco,Nenhum,Nenhum]
 >>> [[Tronco,Tronco,Tronco],[Nenhum,Nenhum]]
 -}
aux3 :: Eq a => [a] -> [[a]]
aux3 [] = []
aux3 (h:t) = (h:takeWhile (== h) t) : aux3 (dropWhile (== h) t)


arvores_maximas_juntas :: Mapa -> Bool
arvores_maximas_juntas (Mapa lr (((Rio vr),(o1:ot)):t)) = arvores_maximas_juntas (Mapa lr t)
arvores_maximas_juntas (Mapa lr (((Estrada ve) ,(o1:ot)):t)) = arvores_maximas_juntas (Mapa lr t)
arvores_maximas_juntas (Mapa lr []) = True
arvores_maximas_juntas (Mapa lr ((Relva ,(o1:ot)):t))
                                  |(o1 == Nenhum) && (length (head (aux4 (dropWhile (== Nenhum) ot))) > 3) = False
                                  |(o1 == Arvore) && ((length (head (aux4 (o1:ot)))) > 3) = False 
                                  |otherwise = arvores_maximas_juntas (Mapa lr t)


troncos_minimos :: Mapa -> Bool
troncos_minimos (Mapa lr ((Relva,(o1:ot)):t)) = troncos_minimos (Mapa lr t)
troncos_minimos (Mapa lr (((Estrada ve) ,(o1:ot)):t)) = troncos_minimos (Mapa lr t)
troncos_minimos (Mapa lr []) = True
troncos_minimos (Mapa lr (((Rio vr) , (o1:ot)):t)) |(o1 == Nenhum) && (length (head (aux4 (dropWhile (== Nenhum) ot))) < 3) = False
                                                   |(o1 == Tronco) && ((length (head (aux4 (o1:ot)))) < 3) = False 
                                                   |otherwise = troncos_minimos (Mapa lr t) 

{-|A função 'mapaValido4' funciona da mesma maneira que a função 'mapaValido3' mas com @Carro@ e com a auxiliar 'aux4' .

==Exemplo de Utilização: 
 >>> mapaValido4 (Mapa 3 [(Estrada 3,[Nenhum,Nenhum,Carro]),(Rio (-1),[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Carro]),(Rio 1,[Tronco,Tronco,Tronco,Nenhum,Nenhum,Tronco,Tronco])])
 >>> True
-}
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

{-|A função auxiliar 'aux4' funciona da mesma maneira que a função 'aux3'.-}
aux4 :: Eq a => [a] -> [[a]]
aux4 [] = []
aux4 (h:t) = (h:takeWhile (== h) t) : aux4 (dropWhile (== h) t)

{-|A função 'mapaValido5' faz com que ,em todas as listas de obstaculos, exista pelo menos um elemento @Nenhum@, ou seja, uma linha não pode ser formada somente por obstáculos

==Exemplo de Utilização: 
 >>> mapaValido5 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco])
 >>> True 

 >>> mapaValido5 (Mapa 3 [(Rio 3,[Tronco,Tronco,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco])
 >>> False
 -}
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

{-|A função 'mapaValido6' recebe um @Mapa@ e o que faz é comparar a largura desse mapa com o comprimento da lista de obstaculos.
Para que um mapa seja válido a largura e o comprimento da lista de obstáculos tem de ser a mesma. 

==Exemplos de Utilização:
 >>> mapaValido6 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco])])
 >>> True 

 >>> mapaValido6 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco,Tronco])])
 >>> False-}
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

{-|A função 'mapaValido7' recebe um mapa e caso existam mais de 5 relvas ou estradas seguidas ou mais de 4 rios seguidos então
o mapa não será valido

==Exemplos de Utilização:
 >>> mapaValido7 (Mapa 3 [(Rio 3,[Nenhum,Nenhum,Tronco]),(Rio (-1),[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Rio 1,[Nenhum,Nenhum,Carro])])
 >>> True 

 >>> mapaValido7 (Mapa 3 [(Relva,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Tronco]),(Relva,[Nenhum,Nenhum,Arvore]),(Relva,[Nenhum,Nenhum,Carro]),(Relva,[Tronco,Tronco,Nenhum,Tronco,Tronco]),(Relva,[Tronco,Tronco,Nenhum,Tronco,Tronco])])
 >>> False-}
mapaValido7 :: Mapa -> Bool
mapaValido7 (Mapa lr []) = True 
mapaValido7 (Mapa lr ((Relva,lo1):(Relva,lo2):(Relva,lo3):(Relva,lo4):(Relva,lo5):(Relva,lo6):t)) = False
mapaValido7 (Mapa lr (((Rio vr),lo):((Rio vr1),lo1):((Rio vr2),lo2):((Rio vr3),lo3):((Rio vr4),lo4):t)) = False
mapaValido7 (Mapa lr (((Estrada ve),lo):((Estrada ve1),lo1):((Estrada ve2),lo2):((Estrada ve3),lo3):((Estrada ve4),lo4):((Estrada ve5),lo5):t)) = False
mapaValido7 (Mapa lr ((tr,lo):t)) = mapaValido7 (Mapa lr t)
          
                                     