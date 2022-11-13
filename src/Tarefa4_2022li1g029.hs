{- |
Module      : Tarefa4_2022li1g029
Description : Determinar se o jogo terminou
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g029 where

import LI12223

{-|Na realização desta tarefa usei algumas funções auxiliares para chegar ao objetivo final.
Eu utilizei 3 funções auxiliares que recebem os mesmos tipos que a função principal 'jogoTerminou' sendo estas a 'jogoTerminou1' , 'jogoTerminou2' e 'jogoTerminou3'.

 
A Função 'jogoTerminou' é uma função que recebe um @Jogo@ e diz se o jogador morreu ,ou seja, o valor da função será de @True@ ou
se continua vivo ,ou seja, o valor da função será de @False@ .  


==Definição Dos Tipos:
>>> :: Jogo 
>>> -> Bool 

==Exemplos de Utilização:
>>> jogoTerminou (Jogo (Jogador (2,2)) (Mapa 5 [(Estrada 3 ,[Nenhum,Nenhum,Carro]),(Relva,[Arvore,Nenhum,Nenhum]),(Estrada 3,[Nenhum,Nenhum,Carro])]))
>>> True 
-}
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa lr l)) = jogoTerminou1 (Jogo (Jogador (x,y)) (Mapa lr l)) || jogoTerminou2 (Jogo (Jogador (x,y)) (Mapa lr l)) || jogoTerminou3 (Jogo (Jogador (x,y)) (Mapa lr l))

{-|A função 'jogoTerminou1' recebe um Jogo e caso o jogador saia do mapa o jogo acaba.

==Exemplos de Utilização:
*Caso o jogador saia pelo lado esquerdo do mapa:
 @ 
  >>> jogoTerminou1 (Jogo (Jogador ((-1),1)) (Mapa 5 [(Relva,[Nenhum,Nenhum,Arvore])]))
  >>> True 
 @

*Caso o jogador saia pelo lado direito do mapa:
@ 
 >>> jogoTerminou1 (Jogo (Jogador (4,4)) (Mapa 4 [(Relva,[Nenhum,Nenhum,Arvore])]))
 >>> True 
@
*Caso seja apanhado:
@
 >>> jogoTerminou1 (Jogo (Jogador (5,4)) (Mapa 5 [(Rio 3 ,[Nenhum,Nenhum,Tronco])]))
 >>> True 
@ 
-} 

jogoTerminou1 :: Jogo -> Bool 
jogoTerminou1 (Jogo (Jogador (x,y)) (Mapa lr l)) = x > (lr-1) || x < 0 || y > ((length l)-1)

{-|A Função 'jogoTerminou2' recebe um Jogo e se o jogador não estiver em cima de um tronco
no rio ele morre e o jogo termina.

==Exemplos de Ulilização:
 >>> jogoTerminou2 (Jogo (Jogador (2,0)) (Mapa 5 [(Rio 3 ,[Tronco,Tronco,Nenhum])]))
 >>> True  

 >>> jogoTerminou2 (Jogo (Jogador (1,0)) (Mapa 5 [(Rio 3 ,[Tronco,Tronco,Nenhum])]))
 >>> False-}
                                
jogoTerminou2 :: Jogo -> Bool
jogoTerminou2 (Jogo (Jogador (x,y)) (Mapa lr [])) = False 
jogoTerminou2 (Jogo (Jogador (x,y)) (Mapa lr ((tr,lo):t))) |y==0 && (hanenhum x lo) = True 
                                                           |otherwise = jogoTerminou2 (Jogo (Jogador (x,y-1)) (Mapa lr t))

{-|A função 'hanenhum' é uma função auxiliar de 'jogoTerminou2' que recebe a coordenada x do lugar onde o jogador está
e a lista de obstaculos e devolve um booliano, esta função serve para ver se existe ou não,um tronco no lugar onde o jogador está.

==Exemplos de Utilização:
 >>> hanenhum 2 [Tronco,Nenhum,Nenhum]
 >>> True 

 >>> hanenhum 0 [Tronco,Nenhum,Nenhum]
 >>> False -}

hanenhum :: Int->[Obstaculo]->Bool
hanenhum x [] =False
hanenhum x (o1:ot) |x==0 && o1==Nenhum=True
                   |otherwise = hanenhum (x-1) ot

{-|A Função 'jogoTerminou3' recebe um Jogo e se o jogador estiver no mesmo lugar que um carro
ele morre e o jogo termina.

==Exemplos de Utilização:
 >>> jogoTerminou3 (Jogo (Jogador (1,0)) (Mapa 5 [(Estrada 3 ,[Carro,Nenhum,Nenhum])]))
 >>> False 

 >>> jogoTerminou3 (Jogo (Jogador (0,0)) (Mapa 5 [(Estrada 3 ,[Carro,Nenhum,Nenhum])]))
 >>> True-}

jogoTerminou3 :: Jogo -> Bool 
jogoTerminou3 (Jogo (Jogador (x,y)) (Mapa lr [])) = False 
jogoTerminou3 (Jogo (Jogador (x,y)) (Mapa lr ((tr,lo):t))) |y==0 && hacarro x lo = True 
                                                           |otherwise = jogoTerminou3 (Jogo (Jogador (x,y-1)) (Mapa lr t))

{-|A função 'hanenhum' é uma função auxiliar de 'jogoTerminou3' que recebe a coordenada x do lugar onde o jogador está
e a lista de obstaculos e devolve um booliano, esta função serve para ver se existe ou não, um carro no lugar onde o jogador está. 

==Exeplos de Utilização: 
 >>> hacarro 2 [Nenhum, Nenhum,Carro]
 >>> True 

 >>> hacarro 1 [Nenhum,Nenhum,Carro]
 >>> False-}

hacarro:: Int->[Obstaculo]->Bool
hacarro x [] = False
hacarro x (o1:ot) |x==0 && o1==Carro =True
                  |otherwise = hacarro (x-1) ot
