{- |
Module      : Main
Description : Funções que formam o jogo
Copyright   : Vitor Miguel Almeida RIbeiro <a104010@alunos.uminho.pt>
              Gonçalo Freitas <a104350@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Main where

import LI12223
import Tarefa1_2022li1g029
import Tarefa2_2022li1g029
import Tarefa3_2022li1g029
import Tarefa4_2022li1g029
import Tarefa5_2022li1g029


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import System.Exit
import System.Random


data Opção1 = Play
            |Jogo_Salvo
            |Simulater
            |Sair
           deriving (Show,Eq)
data Opção2 = Resume 
            |Save_and_Quit
            |Quit
           deriving (Show,Eq)

data Menu = Opcoes Opção1 
          |ModoJogo
          |Bot
          |PerdeuJogo
          |Pause Opção2
         deriving (Show,Eq)

data Skin = Kid | Warrior | Zelda
         deriving (Show,Eq)
type Pontuação = Int 

type Time = Float 

type Images = [Picture]
type Seed= Int

type World =     (Menu,Jogo,Skin,Images,Time,Pontuação,Seed)
{-| A função window vai criar a janela do jogo, recebendo o título da janela e a largura e altura da mesma e as coordenadas da tela onde estará-}

window :: Display 
window = InWindow "CrossyRoad: The Indie Game" (950,950) (0,0)

{-| A função fr dá a frequência que o em que o jogo irá rodar.-}
fr :: Int
fr = 80

{-| A função initialState recebe uma imagem e uma seed e dá um mundo utilizando as imagens -}

initialState :: Images ->Seed->World
initialState images seed = (Opcoes Play,jogoinit,Kid,images,0,0,seed)

{-| A função jogoinit recebe um jogo que será utilizado para ser o jogo inicial sempre que o jogador clica em play-}

jogoinit::Jogo
jogoinit = ( Jogo (Jogador (9,5)) (Mapa 19 [(e 2,[n,n,n,n,c,n,n,c,n,n,n,n,n,n,n,n,n,n,n]),   --(10,9)
                                            (r,[n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n]),
                                            (r,[n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n]),
                                            (r,[a,a,a,a,a,a,n,n,n,n,n,n,n,a,a,a,a,a,a]), 
                                            (r,[a,a,a,a,a,a,n,n,n,n,n,n,n,a,a,a,a,a,a]), 
                                            (r,[a,a,a,a,a,a,n,n,n,n,n,n,n,a,a,a,a,a,a]),
                                            (r,[a,a,a,a,a,a,n,n,n,n,n,n,n,a,a,a,a,a,a]),
                                            (r,[a,a,a,a,a,a,n,n,n,n,n,n,n,a,a,a,a,a,a]),
                                            (r,[n,n,n,n,n,a,n,n,n,n,n,n,n,a,n,n,n,n,n]), 
                                            (r,[n,n,n,n,n,n,a,n,n,n,n,n,a,n,n,n,n,n,n]), 
                                            (r,[n,n,n,n,n,n,a,n,n,n,n,n,a,n,n,n,n,n,n]),
                                            (r,[n,n,n,n,n,n,a,a,a,a,a,a,a,n,n,n,n,n,n]),
                                            (r,[n,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,a,n,n]), 
                                            (r,[n,a,a,a,n,n,n,n,n,n,n,n,n,n,n,a,a,a,n]),
                                            (r,[a,a,a,a,a,n,n,n,n,n,n,n,n,n,a,a,a,a,a]),
                                            (r,[n,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,a,n,n]), 
                                            (r,[n,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,a,n,n]),
                                            (r,[n,n,a,n,n,n,n,n,n,n,n,n,n,n,n,n,a,n,n]),
                                            (r,[a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a]),
                                            (r,[a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a]), 
                                            (r,[a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a]), 
                                            (r,[a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a,a])]))
                                             where a = Arvore
                                                   n = Nenhum
                                                   c = Carro 
                                                   t = Tronco 
                                                   r = Relva
                                                   ri = Rio
                                                   e = Estrada
{-|A função desenhaestado recebe um mapa e dependendo da variável menu que recebe desenha o que aparecerá na tela do jogo-}
desenhaestado :: World ->IO Picture
desenhaestado (PerdeuJogo,_, _, images,_,p,_) =return $ Pictures [last images, Scale (1) (1) $ Translate 0 50 $ last (take 2( reverse images))   , Translate (-200) (350)$ scale (0.5) (0.5) $ Text ("Final Score:" ++ (show p)), Translate 150 (-220) $  Scale (1.5) (1.5) $ last (take 3( reverse images))]                                                                                                                                                                          --desenha o estado perdeujogo
desenhaestado (Pause Resume ,_,_, images,_,p,_)= return $ Pictures [last images ,desenhapause, Color red $ desenharesume,desenhaquit_save,desenhaquit,desenhascore p]   
desenhaestado (Pause Save_and_Quit ,_,_, images,_,p,_) = return $ Pictures [last images ,desenharesume ,Color red $ desenhaquit_save, desenhapause,desenhaquit,desenhascore p]     
desenhaestado (Pause Quit ,_,_,images,_,p,_)= return $ Pictures [last images, desenhapause,desenharesume, desenhaquit_save, Color red $ desenhaquit,desenhascore p]   
desenhaestado (Opcoes Play, jogo,skin, images,_,p,_) = return $ Pictures [last images,desenha_keys,desenha_texto_skin,Scale (0.5) (0.5) $ Translate 600 (-750) $ imageindex images 13,Scale (0.5) (0.5) $ Translate 600 (-500) $ imageindex images 14,Color red $ drawplay , desenhajogo_salvo, drawsimulater,drawquit,desenhaskinmenu skin images] 
desenhaestado (Opcoes Jogo_Salvo, jogo,skin, images,_,p,_) = return $ Pictures [last images,desenha_keys,desenha_texto_skin,Scale (0.5) (0.5) $ Translate 600 (-750) $ imageindex images 13,Scale (0.5) (0.5) $ Translate 600 (-500) $ imageindex images 14,drawplay ,Color red $ desenhajogo_salvo, drawsimulater,drawquit,desenhaskinmenu skin images]
desenhaestado (Opcoes Simulater, jogo,skin, images,_,p,_) =return $  Pictures [last images,desenha_keys,desenha_texto_skin,Scale (0.5) (0.5) $ Translate 600 (-750) $ imageindex images 13,Scale (0.5) (0.5) $ Translate 600 (-500) $ imageindex images 14,drawplay ,Color red $ drawsimulater,desenhajogo_salvo, drawquit,desenhaskinmenu skin images]                                                                                       
desenhaestado (Opcoes Sair, jogo,skin, images,_,p,_) = return $ Pictures [last images,desenha_keys,desenha_texto_skin,Scale (0.5) (0.5) $ Translate 600 (-750) $ imageindex images 13,Scale (0.5) (0.5) $ Translate 600 (-500) $ imageindex images 14,drawplay ,drawsimulater,desenhajogo_salvo, Color red $ drawquit,desenhaskinmenu skin images]                                                                                      
desenhaestado (ModoJogo,(Jogo (Jogador (x,y)) (Mapa lar l)),skin,images,t,p,_)= return $ Pictures [(desenhaterrenos (listaterrenos (agrupaestradas l) 500 t)),(desenhaobstaculos (listaobstaculos (Mapa lar l) (-450) 500 t images)),(desenhaplayer x y t skin images),drawPoints p,Translate (-400) 300 $ desenhapause ]
desenhaestado (Bot,(Jogo (Jogador (x,y)) (Mapa lar l)),skin,images,t,p,_)= return $ Pictures [(desenhaterrenos (listaterrenos (agrupaestradas l) 500 t)),(desenhaobstaculos (listaobstaculos (Mapa lar l) (-450) 500 t images)),(desenhaplayer x y t skin images),drawPoints p ]


drawplay = Translate (-200) (-200) $ drawOption "PLAY"
drawsimulater = Translate (-200) (-340) $ drawOption "Simulater"
drawquit = Translate (-200) (-410) $ drawOption "Quit"
desenhascore p = Translate (-420) 420 $ scale (0.3) (0.3) $ Text ("Your score: " ++ (show p))
desenhapause = scale (1.3) (1.3) $ Pictures [Color red $ Translate (-10) (100) $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Color red $ Translate 20 100 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)]]
desenharesume = Translate (-50) (-100) $ Scale (0.7) (0.7) $ drawOption "Resume"
desenhaquit_save = Translate (-50) (-150) $ scale (0.7) (0.7) $ drawOption "Quit and Save"
desenhaquit = Translate (-50) (-200) $ scale (0.7) (0.7) $ drawOption "Quit"
desenhajogo_salvo = Translate (-200) (-270) $ drawOption "Jogo Salvo"
desenha_keys = Scale (0.5) (0.5) $ Translate 450 (-450) $ drawOption "KEYS :"
desenha_texto_skin = Color red $ Scale (0.6) (0.6) $ Translate 270 220 $ drawOption "SKINS :"
{-| A função da imagemindex é uma função que vai receber uma lista de imagens e um inteiro que será a posição onde está a imagem nessa lista , tendo em atenção que a primeira imagem está no 0 -}

imageindex::[Picture]->Int->Picture
imageindex (x:_) 0 = x 
imageindex (_:xs) n = imageindex xs (n-1)


desenhaskinmenu Kid images= Pictures [Translate 300 20 $ scale 5 5 $ imageindex images 10,  Translate 270 150 $ scale (0.4) (0.3) $ Text "Kid", Translate 300 0 $ color red $ rectangleWire 200 250 ]
desenhaskinmenu Warrior images= Pictures [Translate 295 0 $ scale 5 5 $ imageindex images 11, Translate 225 150 $ scale (0.4) (0.3) $ Text "Warrior", Translate 300 0 $ color red $ rectangleWire 200 250 ]   
desenhaskinmenu _ images= Pictures [Translate 295 (-5) $ scale 6 6 $ imageindex images 12, Translate 250 150 $ scale (0.4) (0.3) $ Text "Zelda",Translate 300 0 $ color red $ rectangleWire 200 250 ] 

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p |p<100 = Translate 400 400 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |p>100 && p<1000 = Translate 380 400 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |p>1000 && p <10000 = Translate 350 400 $ color red $ Scale (0.4) (0.4) $ Text (show p)
             |otherwise = Translate 360 400 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
{-|A função desenhaplayer recebe as coordenadas do player e desenha a skin escolhida no local do jogador -}
desenhaplayer::Int->Int->Float->Skin->[Picture]->Picture
desenhaplayer x y t skin images = (Translate ((i*50)-450) (500 -(j*50)-(t)) $ desenhaskin skin t images)
                            where i=fromIntegral x 
                                  j=fromIntegral y
{-|A função desenhaskin recebe uma skin e um   -}
desenhaskin::Skin->Float->[Picture]->Picture
desenhaskin Kid t images = if t<25 then imageindex images 0 else Translate 0 0 $ imageindex images 1
desenhaskin Warrior t images = if t<25 then Translate 0 15 $ imageindex images 6 else Translate 0 15 $ imageindex images 7 
desenhaskin Zelda t images = if t<25 then Translate 0 10 $ imageindex images 8 else Translate 0 10 $ imageindex images 9 

{-|A função agrupaestradas recebe uma lista de terrenos com obstaculos e sempre que o terreno e a auxiliar exiteestrada vai ver se o obstaculo é estrada e se for ele vai agrupar com os outros obstaculos estradas 

==Exemplos: 
>>> agrupaestradas [(Estrada 2, [Nenhum,Nenhum]), (Estrada 1, [Nenhum,Nenhum])] 
>>> agrupaestradas [[Estrada 2],[Estrada 1]]-}
agrupaestradas::[(Terreno,[Obstaculo])]->[[Terreno]]
agrupaestradas []= []
agrupaestradas [(te,obs)]=[[te]]
agrupaestradas ((Estrada v,obs):tf)|existeestrada (head (agrupaestradas tf)) = ((Estrada v):(head (agrupaestradas tf))):(tail (agrupaestradas tf))
                                   |otherwise = [Estrada v]:agrupaestradas tf
agrupaestradas (h:t)=[fst h]:agrupaestradas t                                            
    

{-|É auxiliar de agrupa os obstaculos. -}                                       
existeestrada::[Terreno]->Bool
existeestrada []=False 
existeestrada ((Estrada v):tf) = True 
existeestrada (_:tf) =existeestrada tf 

    
{-| A listaterrenos cria uma lista de de listas de terrenos  -}
listaterrenos::[[Terreno]]->Float->Float->[([Terreno],Float,Float)]
listaterrenos [] _ _ = []
listaterrenos (te:tf) a t = (te,a,t):listaterrenos tf (a-(50*l)) t
                                   where l = fromIntegral (length te) 

desenhaterrenos::[([Terreno],Float,Float)]->Picture
desenhaterrenos l = Pictures (map f l) 
                         where f (te,a,t) = Translate 0 (a-t) $ (lfundo te) 

{-|A função listaobstaculos basicamente diz onde o obstaculo vai ficar  -}
listaobstaculos::Mapa->Float->Float->Float->Images->[(Terreno,Obstaculo,Float,Float,Images)]
listaobstaculos (Mapa lar []) _ _ _ _ = []
listaobstaculos (Mapa lar ((te,[]):tf)) l a t textures = listaobstaculos (Mapa lar (tf)) (-450) (a-50) t textures
listaobstaculos (Mapa lar ((te,(ob1:obs)):tf)) l a t textures = (te,ob1,l,a-t,textures):listaobstaculos (Mapa lar ((te,obs):tf)) (l+50) a t textures 
                                                                  
{-|Esta função vai dizer o obstaculo e a posição e vai criar uma imagem.-}
desenhaobstaculos::[(Terreno,Obstaculo,Float,Float,Images)]->Picture
desenhaobstaculos l = Pictures (map f l) 
              where f (te,obs,l,a,textures) = Translate l a $ (obj te obs textures) 
{-|A função obj -}
obj::Terreno->Obstaculo->Images->Picture  
obj (Rio v) Tronco images = Translate 0 (-50) $ imageindex images 2  
obj (Estrada v) Carro images= if v<0 then  Translate 0 (-15) $ imageindex images 4 else Translate 10 0 $ imageindex images 5
obj (Relva) Arvore images = Translate 5 (-10) $ imageindex images 3 
obj _ _ images = Blank 



lfundo::[Terreno]->Picture
lfundo [(Rio v)]= color blue $ rectangleSolid 950 50
lfundo [(Relva)]= color green $ rectangleSolid 950 50
lfundo [(Estrada v)]=color (greyN (0.2)) $ rectangleSolid 950 50
lfundo [(Estrada v),(Estrada v1)]= Pictures [color (greyN (0.2)) $ Translate 0 (-25) $ rectangleSolid 950 100,Translate 0 (-25) $ (desenhalinhasestrada 0)]
lfundo [(Estrada v),(Estrada v1),(Estrada v2)]= Pictures [color (greyN (0.2)) $ Translate 0 (-50) $ rectangleSolid 950 150 ,Translate 0 (-25)  $ (desenhalinhasestrada 1),Translate 0 (-75)  $ (desenhalinhasestrada 1) ]
lfundo [(Estrada v),(Estrada v1),(Estrada v2),(Estrada v3)]= Pictures [color (greyN (0.2)) $  Translate 0 (-75) $ rectangleSolid 950 200,Translate 0 (-25) $ (desenhalinhasestrada 2),Translate 0 (-75) (desenhalinhasestrada 2),Translate 0 (-125)  $ (desenhalinhasestrada 2)]
lfundo [(Estrada v),(Estrada v1),(Estrada v2),(Estrada v3),(Estrada v4)]= Pictures [color (greyN (0.2)) $  Translate 0 (-100) $ rectangleSolid 950 250,Translate 0 (-25) $ (desenhalinhasestrada 3),Translate 0 (-75) (desenhalinhasestrada 3),Translate 0 (-125)  $ (desenhalinhasestrada 3),Translate 0 (-155)  $ (desenhalinhasestrada 3)]

{-|A função desenhalinhaestrada caso tenha 2 estradas seguidas desenha a linha a trassejado caracteristica da estrada-}
desenhalinhasestrada::Float->Picture
desenhalinhasestrada n = Pictures [Translate (-300-(50*n)) 0 $ linha ,Translate (-150-(25*n)) 0 $ linha ,linha,Translate (150+(25*n)) 0 $ linha ,Translate (300+(50*n)) 0 $ linha ]
                            where linha = color white $ polygon [(-25,1),(-25,-1),(25,-1),(25,1)]
{-|As funçoes key...menu são funções que recebem um mundo e dependendo da opção aquilo dá um mundo no menu do jogo-}
key_up_menu :: World -> World
key_up_menu (Opcoes op, jogo,skin,i,n,p,r) = case op of
    Play -> (Opcoes Sair, jogo,skin,i,n,p,r)
    Sair -> (Opcoes Simulater, jogo,skin,i,n,p,r)
    Simulater -> (Opcoes Jogo_Salvo, jogo,skin,i,n,p,r) 
    Jogo_Salvo -> (Opcoes Play,jogo,skin,i,n,p,r)

key_down_menu :: World -> World 
key_down_menu (Opcoes op, jogo,skin,i,n,p,r) = case op of
      Play -> (Opcoes Jogo_Salvo, jogo,skin,i,n,p,r)  
      Simulater -> (Opcoes Sair, jogo,skin,i,n,p,r)     
      Sair -> (Opcoes Play, jogo,skin,i,n,p,r) 
      Jogo_Salvo -> (Opcoes Simulater,jogo,skin,i,n,p,r)

key_space_menu :: World -> World
key_space_menu (Opcoes op, jogo,skin,i,n,p,r) = case op of 
          Play -> (ModoJogo, jogoinit,skin,i,n,0,r)
          Jogo_Salvo -> (ModoJogo, jogo, skin,i,n,p,r)
          Simulater -> (Bot, jogoinit,skin,i,0,0,r+1)
          
          
key_left_menu :: World -> World
key_left_menu (Opcoes op, jogo,skin,i,n,p,r) = case skin of
          Kid -> (Opcoes op, jogo,Warrior,i,n,p,r)
          Warrior -> (Opcoes op, jogo,Zelda,i,n,p,r)
          Zelda -> (Opcoes op, jogo,Kid,i,n,p,r)

key_right_menu :: World -> World
key_right_menu (Opcoes op, jogo,skin,i,n,p,r) = case skin of 
          Kid -> (Opcoes op, jogo,Zelda,i,n,p,r)
          Warrior -> (Opcoes op, jogo,Kid,i,n,p,r)
          Zelda -> (Opcoes op,jogo,Warrior,i,n,p,r)
{-|A função type_key_menu esche qual a função auxiliar a usar dependendo da key usada-}
type_key_menu:: SpecialKey -> World -> World
type_key_menu key ops @(Opcoes op, jogo,skin,i,n,p,r) 
                |testedakey_menu key = case key of 
                       KeySpace -> key_space_menu ops
                       KeyUp -> key_up_menu ops
                       KeyDown -> key_down_menu ops 
                       KeyLeft -> key_left_menu ops 
                       KeyRight -> key_right_menu ops
                |otherwise = (Opcoes op, jogo,skin,i,n,p,r) 

{-|A função testa se a key é valida dentro se uma lista 
==Exemplo: 
>>>testedakey_menu KeySpace
>>>True 

>>>testedakey_menu KeyEnter
>>>False  -}
testedakey_menu :: SpecialKey -> Bool
testedakey_menu key = any (==key) [KeySpace,KeyUp,KeyDown,KeyLeft,KeyRight]


{-|As funçoes key...jogando são funções que recebem um mundo e dependendo da opção aquilo dá um mundo no meio do jogo -}
key_esq_jogando :: World -> World 
key_esq_jogando (ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r)  = (Pause Resume,(Jogo (Jogador (x,y)) mapa) ,skin,i,n,p,r)
          


                                                                       

key_up_modojogo:: World -> World 
key_up_modojogo (ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r) 
                        |(haarvore2 (Jogo (Jogador (x,y-1)) mapa) || y==0) = (ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r)
                        |otherwise = (ModoJogo,Jogo (Jogador (x,y-1)) mapa,skin,i,n,p,r)
                                                            

key_down_modojogo :: World -> World
key_down_modojogo (ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r) 
                        |(haarvore2 (Jogo (Jogador (x,y+1)) mapa)) = (ModoJogo,Jogo (Jogador (x,y)) mapa,skin,i,n,p,r)
                        |otherwise = (ModoJogo,Jogo (Jogador (x,y+1)) mapa,skin,i,n,p,r)

key_left_modojogo :: World -> World 
key_left_modojogo (ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r) 
                        |(haarvore2(Jogo (Jogador (x-1,y)) mapa)) = (ModoJogo,Jogo (Jogador (x,y)) mapa,skin,i,n,p,r)
                        |otherwise = (ModoJogo,Jogo (Jogador (x-1,y)) mapa,skin,i,n,p,r) 

key_right_modojogo:: World -> World
key_right_modojogo (ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r)
                        |(haarvore2 (Jogo (Jogador (x+1,y)) mapa)) = (ModoJogo,Jogo (Jogador (x,y)) mapa,skin,i,n,p,r) 
                        |otherwise = (ModoJogo,Jogo (Jogador (x+1,y)) mapa,skin,i,n,p,r)

type_key_modojogo :: SpecialKey -> World -> World
type_key_modojogo key ops2 @(ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r) 
                                    |testedakey_modojogo key = case key of   -- SERVE PARA QUE NÃO CRASH O JOGO  
                                                          KeyEsc -> key_esq_jogando ops2
                                                          KeyUp -> key_up_modojogo ops2
                                                          KeyDown -> key_down_modojogo ops2
                                                          KeyLeft -> key_left_modojogo ops2
                                                          KeyRight -> key_right_modojogo ops2
                                    |otherwise = (ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r)

testedakey_modojogo:: SpecialKey -> Bool
testedakey_modojogo key = any (==key) [KeyDown,KeyUp,KeyRight,KeyLeft,KeyEsc] --testa se a key está naquele intervalo 

--Pause
key_up_pausa :: World -> World
key_up_pausa (Pause op2,jogo,skin,i,n,p,r) = case op2 of 
                     Resume -> (Pause Quit, jogo ,skin,i,n,p,r)
                     Quit -> (Pause Save_and_Quit, jogo ,skin,i,n,p,r)
                     Save_and_Quit -> (Pause Resume, jogo ,skin,i,n,p,r)


key_down_pausa :: World -> World
key_down_pausa (Pause op2,jogo,skin,i,n,p,r) = case op2 of 
                     Resume -> (Pause Save_and_Quit, jogo ,skin,i,n,p,r)
                     Quit -> (Pause Resume, jogo ,skin,i,n,p,r)
                     Save_and_Quit -> (Pause Quit, jogo ,skin,i,n,p,r)

key_space_pausa :: World -> World 
key_space_pausa (Pause op2,jogo,skin,i,n,p,r) = case op2 of 
                Resume -> (ModoJogo , jogo,skin,i,n,p,r)
                Quit -> (Opcoes Play,jogoinit,skin,i,n,0,r)
                Save_and_Quit -> (Opcoes Play,jogo,skin,i,n,p,r)

type_key_pausa :: SpecialKey -> World-> World
type_key_pausa key ops3 @(Pause op2,jogo,skin,i,n,p,r) 
                            |testedakey_pausa key = case key of 
                                    KeyUp -> key_up_pausa ops3
                                    KeyDown -> key_down_pausa ops3
                                    KeySpace -> key_space_pausa ops3
                            |otherwise = (Pause op2,jogo,skin,i,n,p,r)


testedakey_pausa :: SpecialKey -> Bool 
testedakey_pausa key = any (==key) [KeyUp,KeyDown,KeySpace]
--
event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey key) Down _ _) ops3 @(Pause op2,jogo,skin,i,n,p,r) = return $ type_key_pausa key ops3
event (EventKey (SpecialKey key) Down _ _) ops2 @(ModoJogo,(Jogo (Jogador (x,y)) mapa),skin,i,n,p,r) = return $ type_key_modojogo key ops2
event (EventKey (SpecialKey key) Down _ _) ops @(Opcoes op, jogo,skin,i,n,p,r) = return $ type_key_menu key ops
event (EventKey (SpecialKey key) Down _ _) (menu, jogo,skin,i,n,p,r) |key ==KeySpace && menu == PerdeuJogo = return $ (Opcoes Play,jogoinit,skin,i,n,0,r)
                                                                     |key == KeySpace && menu == Opcoes Sair = do exitSuccess

event _ w = return $ w
--- ===
animaplayer::Jogo->Jogador 
animaplayer (Jogo jog mapa)=jog 

estanotronco::Jogo -> Bool
estanotronco (Jogo (Jogador (x,0)) (Mapa l ((te,obs):tf))) = hatronco x obs 
estanotronco (Jogo (Jogador (x,y)) (Mapa l ((te,obs):tf))) |y<0=False 
                                                           |otherwise= estanotronco (Jogo (Jogador (x,y-1)) (Mapa l (tf))) 

haarvore2::Jogo ->Bool
haarvore2 (Jogo (Jogador (x,0)) (Mapa l ((te,obs):tf))) = haarvore x obs 
haarvore2 (Jogo (Jogador (x,y)) (Mapa l ((te,obs):tf))) |y<0=False 
                                                        |otherwise= haarvore2 (Jogo (Jogador (x,y-1)) (Mapa l (tf))) 


time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),skin,i,49,p,r) 
                            |jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))  = return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),skin,i,0,p,r+1) 
                            |otherwise = return $ (ModoJogo,verificamapa (deslizaJogo (p-y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) Parado)) (r+mod p (x+y)),skin,i, 0,p+1,r+1)  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),skin,i,t,p,r) 
                            |jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))  = return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),skin,i,0,p,r) 
                            |otherwise = return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) ,skin,i,t+1,p+1,r)
time f (Bot, (Jogo (Jogador (x, y)) (Mapa l to)),skin,i,49,p,r) 
                            |jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))  = return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),skin,i,0,p,r+1) 
                            |otherwise = return $ (Bot,verificamapa (deslizaJogo (p-y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (botplay r) )) (r+mod p (x+y)),skin,i, 0,p+1,r+1)  
                                                                        where botplay r = ([Parado,(Move Cima),(Move Baixo),(Move Esquerda),(Move Baixo)] !! mod r 5) 
time f (Bot, (Jogo (Jogador (x, y)) (Mapa l to)),skin,i,t,p,r) 
                            |jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))  = return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),skin,i,0,p,r+1) 
                            |otherwise = return $ (Bot, (Jogo (Jogador (x, y)) (Mapa l to)) ,skin,i,t+1,p+1,r)
time f w = return $ w



--a função verificamapa além de adicionar uma velocidade aleatoria aos rios/estradas , "filtra" as linhas que não permitem a passagem do jogador
verificamapa::Jogo->Int->Jogo
verificamapa (Jogo x (Mapa l ((Rio v1,ob1):(Rio v2,ob2):resto))) seed = (Jogo x (Mapa l ((Rio (velocidadealeatoria v2 seed),ob1):(Rio v2,ob2):resto)))
verificamapa (Jogo x (Mapa l ((Rio v,ob1):resto))) seed = (Jogo x (Mapa l ((Rio (velocidadealeatoria 0 seed),ob1):resto)))
verificamapa (Jogo x (Mapa l ((Estrada v1,ob1):resto))) seed = (Jogo x (Mapa l ((Estrada (velocidadealeatoria 0 seed),ob1):resto)))
verificamapa (Jogo x (Mapa l ((Relva,ob1):(Relva,ob2):resto))) seed | (vepassagem ob1 ob2 ) && (mapaValido (Mapa l ((Relva,ob1):(Relva,ob2):resto))) = Jogo x (estendeMapa (Mapa l ((Relva,ob2):resto)) seed)
                                                                    |otherwise = verificamapa (Jogo x (Mapa l ((Relva,ob1):(Relva,ob2):resto))) (seed+1)
verificamapa w _ = w 

--a função vepassagem verifica se há uma passagem possivel para o jogador 

vepassagem:: [Obstaculo] -> [Obstaculo] -> Bool
vepassagem xs ys = (Nenhum, Nenhum) `elem` zip xs ys 

--a função velocidade aleatoria 
velocidadealeatoria::Int->Int->Int  
velocidadealeatoria v seed = ((velocidadesvalidas v) !! (mod seed (length(velocidadesvalidas v))))

velocidadesvalidas::Int->[Int]
velocidadesvalidas v |v==0= [-2,-1,1,2]
                     |v<0 = [1,2]
                     |otherwise =[-2,-1] 

main :: IO ()
main = do
 kid1 <- loadBMP "kid1.bmp"
 kid2 <- loadBMP "kid2.bmp"
 log <- loadBMP "tronco.bmp"
 tree <- loadBMP "arvore1.bmp"
 car1 <- loadBMP "carro.bmp"
 car2 <- loadBMP "carro2.bmp"
 warrior1 <- loadBMP "warrior1.bmp"
 warrior2 <- loadBMP "warrior2.bmp"
 zelda1<- loadBMP "zelda1.bmp"
 zelda2<- loadBMP "zelda2.bmp"
 kidmenu<- loadBMP "kidmenu.bmp"
 warriormenu<- loadBMP "warriormenu.bmp"
 zeldamenu<- loadBMP "zeldamenu.bmp"
 fundomenu<- loadBMP "fundo.bmp"
 seed<- randomRIO (0,100)
 game_over <- loadBMP "game_over.bmp" 
 space <- loadBMP "press_space.bmp"
 setas <- loadBMP "setas.bmp"
 espaço <- loadBMP "space.bmp"
 let images = [scale (0.5) (0.5) kid1, scale (0.5) (0.5) kid2,scale (0.3) (0.3) log, tree,scale (0.4) (0.6)  car1,scale (0.2) (0.4)  car2,warrior1,warrior2,zelda1,zelda2,Scale (0.7) (0.7) $ Translate 2 0 kidmenu,warriormenu, scale (1.2) (1.2) zeldamenu, espaço,setas,space,game_over,fundomenu]
 playIO window white  fr (initialState images seed) desenhaestado event time

