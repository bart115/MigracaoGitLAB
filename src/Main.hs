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

{-

data Opção1 = Play
            |Save
            |Sair

data Opção2 = Resume 
            |Quit
           

data Menu = Opcoes Opção1 
          |ModoJogo
          |PerdeuJogo
          |Pause Opção2

type Pontuação = Int 

type Time = Float 

type Images = [Picture]

type World = (Menu,Jogo,Images,Time,Pontuação)

window :: Display 
window = InWindow "CrossyRoad: The Indie Game" (700, 700) (0,0)

fr :: Int
fr = 50

initialState :: Images ->World
initialState images = (Opcoes Play,jogoinit,images, 0,0)

jogoinit::Jogo
jogoinit = ( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio (1),[Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]), 
    (Estrada (-1),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Estrada 1,[Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 2,[Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]), 
    (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
    (Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, images, n,p) =return $ Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "TAKE THE L"                                                                                                                                                                           --desenha o estado perdeujogo
drawState (Pause Resume ,jogo, images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],Translate 300 250 $ scale (0.4) (0.4) $ drawOption "QUIT"]           --desenha o estado pause
drawState (Pause Quit ,jogo,images,n,p)= return $ Pictures [ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 300 250 $ scale (0.5) (0.5) $ drawOption "QUIT"]  -- desenha o estado pause 
drawState (Opcoes Play, jogo, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "PLAY", Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                     --desenha o menu das opçoes para jogar normal
drawState (Opcoes Save, jogo, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "PLAY",Color blue $ Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "PLAY",Translate (-110) (-70) $ drawOption "SAVE", Color blue $ Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa lar l)),images,t,p)= return $ Pictures [(desenhaterrenos (listadeterrenos (Mapa lar l) 400 t)),(desenhaobstaculos (listadeobstaculos (Mapa lar l) (-300) 400 t images)),(Translate ((i*100)-300+((velplayer l y)*t)) (400 -(j*100)-(t)) $ boneco),drawPoints p ]
                                                            where i=fromIntegral x 
                                                                  j=fromIntegral y 
                                                                  boneco = if (t <25 )||(t>50 && t <75) then Translate 0 15 $ (head images) else Translate 0 15 $ (head (tail images ))
drawstate _ = return $ Blank

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p |p<100 = Translate 270 300 $ points
             |p>100 && p<1000 = Translate 260 300 $ points  
             |otherwise = Translate 250 300 $ points 
                      where points = color red $ Scale (0.4) (0.4) $ Text (show p)



velplayer::[(Terreno,[Obstaculo])]->Int->Float
velplayer ((te,obs):xs) 0 = vel te 
                where vel (Rio v) = fromIntegral v
                      vel (Estrada v) = 0 
                      vel Relva = 0
velplayer (x:xs) n = velplayer xs (n-1)
             

listadeterrenos::Mapa->Float->Float->[(Terreno,Float,Float)]
listadeterrenos (Mapa lar []) _ _ = []
listadeterrenos (Mapa lar ((te,obs):tf)) a t= (te,a,t):listadeterrenos (Mapa lar tf) (a-100) t

desenhaterrenos::[(Terreno,Float,Float)]->Picture
desenhaterrenos l = Pictures (map f l) 
                         where f (te,a,t) = Translate 0 (a-t) $ (lfundo te) 

listadeobstaculos::Mapa->Float->Float->Float->Images->[(Terreno,Obstaculo,Float,Float,Images)]
listadeobstaculos (Mapa lar []) _ _ _ _ = []
listadeobstaculos (Mapa lar ((te,[]):tf)) l a t textures = listadeobstaculos (Mapa lar (tf)) (-300) (a-100) t textures
listadeobstaculos (Mapa lar ((te,(ob1:obs)):tf)) l a t textures = (te,ob1,l+((vel te)*t),a-t,textures):listadeobstaculos (Mapa lar ((te,obs):tf)) (l+100) a t textures 
                                                                    where vel (Rio v) = fromIntegral v
                                                                          vel (Estrada v) = fromIntegral v 
                                                                          vel Relva = 0 

desenhaobstaculos::[(Terreno,Obstaculo,Float,Float,Images)]->Picture
desenhaobstaculos l = Pictures (map f l) 
              where f (te,obs,l,a,textures) = Translate l a $ (obj te obs textures) 

obj::Terreno->Obstaculo->Images->Picture  
obj (Rio v) Tronco images = Translate 0 (-100) $ last (init (init (init images)))  
obj (Estrada v) Carro images= if v<0 then  Translate 0 (-30) $ last (init images) else Translate 20 0 $ last images
obj (Relva) Arvore images = Translate 10 (-20) $ last (init (init images)) 
obj _ _ images = Blank 

cinza= makeColor 0 0 0 40

lfundo::Terreno->Picture
lfundo (Rio v)= color blue $ rectangleSolid 700 100
lfundo (Estrada v)= color cinza $ rectangleSolid 700 100
lfundo (Relva)= color green $ rectangleSolid 700 100


event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Play, jogo,i,n,p) = return $ (ModoJogo, jogo,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Save, jogo,i,n,p) = return $ (ModoJogo, jogo,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Play, jogo,i,n,p) = return $ (Opcoes Sair, jogo,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,i,n,p) = return $ (Opcoes Save, jogo,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Save, jogo,i,n,p) = return $ (Opcoes Play, jogo,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Play, jogo,i,n,p) = return $ (Opcoes Save, jogo,i,n,p)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Save, jogo,i,n,p) = return $ (Opcoes Sair, jogo,i,n,p)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,i,n,p) = return $ (Opcoes Play, jogo,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,i,n,p) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,i,n,p) = return $ (Pause Resume, jogo ,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Resume,jogo,i,n,p) =return $  (ModoJogo ,jogo,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Resume,jogo,i,n,p) =return $  (Pause Quit, jogo,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,i,n,p) =return $  (Pause Resume, jogo,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,i,n,p) = return $ (Opcoes Play,jogo,i,n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,i,n,p) =return $  (Opcoes Play,jogoinit,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,i,n,p) = return $ (Opcoes Play,jogoinit,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _)    (ModoJogo, (Jogo jogador mapa),i,n,p)   = 
     return $ (ModoJogo, (Jogo (atualizajogador (animaJogo (Jogo jogador mapa)  (Move Cima))) mapa),i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _)  (ModoJogo, (Jogo jogador mapa),i,n,p) = 
     return $ (ModoJogo, (Jogo (atualizajogador (animaJogo (Jogo jogador mapa)  (Move Baixo))) mapa),i,n,p) 
event (EventKey (SpecialKey KeyLeft) Down _ _)  (ModoJogo, (Jogo (Jogador (x,y)) mapa),i,n,p)   =
     if verificatronco (Jogo (Jogador (x,y)) mapa) then return $ (ModoJogo, (Jogo (Jogador (x-1,y)) mapa),i,n,p) else  return $ (ModoJogo, (Jogo (atualizajogador(animaJogo (Jogo (Jogador (x,y)) mapa )  (Move Esquerda))) mapa),i,n,p) 
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo,(Jogo (Jogador (x,y)) mapa ),i,n,p) =
     if verificatronco (Jogo (Jogador (x,y)) mapa) then return $ (ModoJogo, (Jogo (Jogador (x+1,y)) mapa),i,n,p) else  return $ (ModoJogo, (Jogo (atualizajogador (animaJogo (Jogo (Jogador (x,y)) mapa)  (Move Direita))) mapa),i,n,p) 
event _ w = return w



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,99,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,0,0) else return $ (ModoJogo,atualizavelocidades ((deslizaJogo (p+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) Parado))) (p+x+y),i,0,p+1)  
time f (ModoJogo,jogo,i,t,p) = if jogoTerminou jogo  then return $ (PerdeuJogo,jogo,i,0,0) else return $ (ModoJogo,jogo ,i,t+1,p+1)
time f w = return $ w

atualizajogador::Jogo->Jogador
atualizajogador (Jogo x y)= x 

atualizavelocidades::Jogo->Int->Jogo
atualizavelocidades (Jogo x (Mapa l ((Rio v1,ob1):(Rio v2,ob2):resto))) _ |v2<0 = (Jogo x (Mapa l ((Rio (1),ob1):(Rio v2,ob2):resto)))
                                                                          |otherwise = (Jogo x (Mapa l ((Rio (1),ob1):(Rio v2,ob2):resto)))
atualizavelocidades (Jogo x (Mapa l ((Rio v,ob1):resto))) seed = (Jogo x (Mapa l ((Rio (velocidadealeatoria seed),ob1):resto)))
atualizavelocidades (Jogo x (Mapa l ((Estrada v1,ob1):(Estrada v2,ob2):resto))) _ |v2<0 = (Jogo x (Mapa l ((Estrada 1,ob1):(Estrada v2,ob2):resto)))
                                                                                  |otherwise = (Jogo x (Mapa l ((Estrada (-1),ob1):(Estrada v2,ob2):resto)))
atualizavelocidades (Jogo x (Mapa l ((Estrada v,ob1):resto))) seed = (Jogo x (Mapa l  ((Estrada (velocidadealeatoria seed ),ob1):resto)))
atualizavelocidades w _ = w 


velocidadealeatoria::Int->Int 
velocidadealeatoria seed |mod 2 seed == 0 = 1
                         |otherwise= (-1) 

verificatronco::Jogo -> Bool
verificatronco (Jogo (Jogador (x,0)) (Mapa l ((te,obs):tf))) = if hatronco x obs ==True then True else False
verificatronco (Jogo (Jogador (x,y)) (Mapa l ((te,obs):tf))) = verificatronco (Jogo (Jogador (x,y-1)) (Mapa l (tf))) 





main :: IO ()
main = do
 bonecoesq <- loadBMP "player1.bmp"
 bonecodir <- loadBMP "player2.bmp"
 log <- loadBMP "tronco.bmp"
 tree <- loadBMP "arvore1.bmp"
 car1 <- loadBMP "carro.bmp"
 car2 <- loadBMP "carro3.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (0.6) (0.6) log,scale 2 2 tree,scale (0.8) (1.2)  car1,scale (0.5) (0.8)  car2]
 playIO window white  fr (initialState images) drawState event time
-}

data Opção1 = Play
            |Save
            |Sair

data Opção2 = Resume 
            |Quit
           

data Menu = Opcoes Opção1 
          |ModoJogo
          |PerdeuJogo
          |Pause Opção2


data Skin = Kid 
type Pontuação = Int 

type Time = Float 

type Images = [Picture]

type World = (Menu,Jogo,Jogada,Skin,Images,Time,Pontuação)

window :: Display 
window = InWindow "CrossyRoad: The Indie Game" (950,950) (0,0)

fr :: Int
fr = 80



initialState :: Images ->World
initialState images = (Opcoes Play,jogoinit,Parado,Kid,images, 0,0)

jogoinit::Jogo
jogoinit = ( Jogo (Jogador (10,8)) (Mapa 19 [(Relva,[n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n]),
    (Rio (1),[t,n,t,t,t,n,t,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Estrada (-1),[c,c,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Estrada 2,[c,n,n,n,n,c,n,n,n,n,n,n,n,n,n,n,n,n,n]),
    (Relva,[n,n,n,n,n,a,n,n,n,a,n,n,n,n,n,a,n,n,n]),
    (Rio 2,[t,n,t,t,t,n,t,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Rio (-1),[t,n,t,t,t,n,t,n,n,n,n,n,n,n,n,n,n,n,n]),
    (Relva,[n,n,n,n,n,a,n,n,n,a,n,n,n,n,n,a,n,n,n]),
    (Relva,[n,n,n,n,n,a,n,n,n,a,n,n,n,n,n,a,n,n,n]),
    (Rio (1),[t,n,t,t,t,n,t,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Estrada (-1),[c,c,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Estrada 2,[c,c,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n]),
    (Relva,[n,n,n,n,n,a,n,n,n,a,n,n,n,n,n,a,n,n,n]),
    (Rio 2,[t,n,t,t,t,n,t,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Rio (-1),[t,n,t,t,t,n,t,n,n,n,n,n,n,n,n,n,n,n,n]),
    (Relva,[n,n,n,n,n,a,n,n,n,a,n,n,n,n,n,a,n,n,n]),
    (Relva,[n,n,n,n,n,a,n,n,n,a,n,n,n,n,n,a,n,n,n]),
    (Rio (1),[t,n,t,t,t,n,t,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Estrada (-1),[c,c,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n]), 
    (Estrada 2,[c,c,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n,n])]))
    where a = Arvore
          n = Nenhum
          c = Carro 
          t = Tronco 

drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, _,_, images, n,p) =return $ Pictures [Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "TAKE THE L" , Translate (-200) (-200) $ Text ("Score:" ++ (show p))]                                                                                                                                                                          --desenha o estado perdeujogo
drawState (Pause Resume ,jogo,_,_, images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],Translate 300 250 $ scale (0.4) (0.4) $ drawOption "QUIT"]           --desenha o estado pause
drawState (Pause Quit ,jogo,_,_,images,n,p)= return $ Pictures [ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 300 250 $ scale (0.5) (0.5) $ drawOption "QUIT"]  -- desenha o estado pause 
drawState (Opcoes Play, jogo,_,_, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "PLAY", Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                     --desenha o menu das opçoes para jogar normal
drawState (Opcoes Save, jogo,_,_, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "PLAY",Color blue $ Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo,_,_, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "PLAY",Translate (-110) (-70) $ drawOption "SAVE", Color blue $ Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa lar l)),_,skin,images,t,p)= return $ Pictures [(desenhaterrenos (listaterrenos (Mapa lar l) 500 t)),(desenhaobstaculos (listaobstaculos (Mapa lar l) (-450) 500 t images)),(desenhaplayer x y t skin images),drawPoints p ]
drawstate _ = return $ Blank

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p |p<100 = Translate 400 400 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |p>100 && p<1000 = Translate 400 400 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |otherwise = Translate 400 400 $ color red $ Scale (0.4) (0.4) $ Text (show p) 

desenhaplayer::Int->Int->Float->Skin->[Picture]->Picture
desenhaplayer x y t skin images = (Translate ((i*50)-450) (500 -(j*50)-(t)) $ desenhaskin skin t images)
                            where i=fromIntegral x 
                                  j=fromIntegral y

desenhaskin::Skin->Float->[Picture]->Picture
desenhaskin Kid t images = if t<25 then Translate 0 0 $ imageindex images 0 else Translate 0 0 $ imageindex images 1


imageindex::[Picture]->Int->Picture
imageindex (x:xs) 0 = x 
imageindex (x:xs) n = imageindex xs (n-1)

listaterrenos::Mapa->Float->Float->[(Terreno,Float,Float)]
listaterrenos (Mapa lar []) _ _ = []
listaterrenos (Mapa lar ((te,obs):tf)) a t= (te,a,t):listaterrenos (Mapa lar tf) (a-50) t

desenhaterrenos::[(Terreno,Float,Float)]->Picture
desenhaterrenos l = Pictures (map f l) 
                         where f (te,a,t) = Translate 0 (a-t) $ (lfundo te) 

listaobstaculos::Mapa->Float->Float->Float->Images->[(Terreno,Obstaculo,Float,Float,Images)]
listaobstaculos (Mapa lar []) _ _ _ _ = []
listaobstaculos (Mapa lar ((te,[]):tf)) l a t textures = listaobstaculos (Mapa lar (tf)) (-450) (a-50) t textures
listaobstaculos (Mapa lar ((te,(ob1:obs)):tf)) l a t textures = (te,ob1,l,a-t,textures):listaobstaculos (Mapa lar ((te,obs):tf)) (l+50) a t textures 
                                                                  

desenhaobstaculos::[(Terreno,Obstaculo,Float,Float,Images)]->Picture
desenhaobstaculos l = Pictures (map f l) 
              where f (te,obs,l,a,textures) = Translate l a $ (obj te obs textures) 

obj::Terreno->Obstaculo->Images->Picture  
obj (Rio v) Tronco images = Translate 0 (-50) $ imageindex images 2  
obj (Estrada v) Carro images= if v<0 then  Translate 0 (-15) $ imageindex images 4 else Translate 10 0 $ imageindex images 5
obj (Relva) Arvore images = Translate 5 (-10) $ imageindex images 3 
obj _ _ images = Blank 

cinza= makeColor 0 0 0 40

lfundo::Terreno->Picture
lfundo (Rio v)= color blue $ rectangleSolid 950 50
lfundo (Estrada v)= color cinza $ rectangleSolid 950 50
lfundo (Relva)= color green $ rectangleSolid 950 50


event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Play, jogo,jog,skin,i,n,p) = return $ (ModoJogo, jogo,jog,skin,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Save, jogo,jog,skin,i,n,p) = return $ (ModoJogo, jogo,jog,skin,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Play, jogo,jog,skin,i,n,p) = return $ (Opcoes Sair, jogo,jog,skin,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,jog,skin,i,n,p) = return $ (Opcoes Save, jogo,jog,skin,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Save, jogo,jog,skin,i,n,p) = return $ (Opcoes Play, jogo,jog,skin,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Play, jogo,jog,skin,i,_,_) = return $ (Opcoes Save, jogo,jog,skin,i,0,0)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Save, jogo,jog,skin,i,_,_) = return $ (Opcoes Sair, jogo,jog,skin,i,0,0)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,jog,skin,i,n,p) = return $ (Opcoes Play, jogo,jog,skin,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair,_,_,_,_,_,_) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,jog,skin,i,n,p) = return $ (Pause Resume, jogo ,jog,skin,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Resume,jogo,jog,skin,i,n,p) =return $  (ModoJogo , jogo,jog,skin,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Resume,jogo,jog,skin,i,n,p) =return $  (Pause Quit, jogo ,jog,skin,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,jog,skin,i,n,p) =return $  (Pause Resume, jogo ,jog,skin,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,jog,skin,i,n,p) = return $ (Opcoes Play,jogo,jog,skin,i,n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,_,skin,i,n,p) =return $  (Opcoes Play,jogoinit,Parado,skin,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,_,skin,i,n,p) = return $ (Opcoes Play,jogoinit ,Parado,skin,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,skin,i,n,p)   = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), (Move Cima),skin,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,skin,i,n,p) = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Baixo),skin,i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,skin,i,n,p)   =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Esquerda),skin,i,n,p) 
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,skin,i,n,p) =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Direita) ,skin,i,n,p) 
event _ w = return w



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,skin,i,49,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,skin,i,0,p) else return $ (ModoJogo,atualizavelocidades (deslizaJogo (p-x-y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) jog)) (p+x+y),Parado,skin,i, 0,p+1)  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,skin,i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,skin,i,0,p) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) ,jog,skin,i,t+1,p+1)
time f w = return $ w


atualizavelocidades::Jogo->Int->Jogo
atualizavelocidades (Jogo x (Mapa l ((Rio v1,ob1):(Rio v2,ob2):resto))) _ |v2<0 = (Jogo x (Mapa l ((Rio (1),ob1):(Rio v2,ob2):resto)))
                                                                          |otherwise = (Jogo x (Mapa l ((Rio (1),ob1):(Rio v2,ob2):resto)))
atualizavelocidades (Jogo x (Mapa l ((Rio v,ob1):resto))) seed = (Jogo x (Mapa l ((Rio (velocidadealeatoria seed),ob1):resto)))
atualizavelocidades (Jogo x (Mapa l ((Estrada v1,ob1):(Estrada v2,ob2):resto))) _ |v2<0 = (Jogo x (Mapa l ((Estrada 1,ob1):(Estrada v2,ob2):resto)))
                                                                                  |otherwise = (Jogo x (Mapa l ((Estrada (-1),ob1):(Estrada v2,ob2):resto)))
atualizavelocidades (Jogo x (Mapa l ((Estrada v,ob1):resto))) seed = (Jogo x (Mapa l  ((Estrada (velocidadealeatoria seed ),ob1):resto)))
atualizavelocidades w _ = w 


velocidadealeatoria::Int->Int 
velocidadealeatoria seed |mod 2 seed == 0 = 1
                         |otherwise= (-1) 






main :: IO ()
main = do
 bonecoesq <- loadBMP "player1.bmp"
 bonecodir <- loadBMP "player2.bmp"
 log <- loadBMP "tronco.bmp"
 tree <- loadBMP "arvore1.bmp"
 car1 <- loadBMP "carro.bmp"
 car2 <- loadBMP "carro3.bmp"
 let images = [scale (0.5) (0.5) bonecoesq, scale (0.5) (0.5) bonecodir,scale (0.3) (0.3) log, tree,scale (0.4) (0.6)  car1,scale (0.2) (0.4)  car2]
 playIO window white  fr (initialState images) drawState event time
{-
data Opção1 = Play
            |Save
            |Sair

data Opção2 = Resume 
            |Quit
           

data Menu = Opcoes Opção1 
          |ModoJogo
          |PerdeuJogo
          |Pause Opção2

type Pontuação = Int 

type Time = Float 

type Images = [Picture]

type World = (Menu,Jogo,Jogada,Images,Time,Pontuação)

window :: Display 
window = InWindow "CrossyRoad: The Indie Game" (700, 700) (0,0)

fr :: Int
fr = 130



initialState :: Images ->World
initialState images = (Opcoes Play,jogoinit,Parado,images, 0,0)

jogoinit::Jogo
jogoinit = ( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio (1),[Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]), 
    (Estrada (-1),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Estrada 2,[Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 2,[Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]), 
    (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
    (Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, _, images, n,p) =return $ Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "TAKE THE L"                                                                                                                                                                           --desenha o estado perdeujogo
drawState (Pause Resume ,jogo,_, images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],Translate 300 250 $ scale (0.4) (0.4) $ drawOption "QUIT"]           --desenha o estado pause
drawState (Pause Quit ,jogo,_,images,n,p)= return $ Pictures [ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 300 250 $ scale (0.5) (0.5) $ drawOption "QUIT"]  -- desenha o estado pause 
drawState (Opcoes Play, jogo,_, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "PLAY", Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                     --desenha o menu das opçoes para jogar normal
drawState (Opcoes Save, jogo,_, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "PLAY",Color blue $ Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo,_, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "PLAY",Translate (-110) (-70) $ drawOption "SAVE", Color blue $ Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa lar l)),_,images,t,p)= return $ Pictures [(mapa2 (mapa1 (Mapa lar l) 400 t)),(mapa4 (mapa3 (Mapa lar l) (-300) 400 t images)),(Translate ((i*100)-300+((velplayer l y)*t)) (400 -(j*100)-(t)) $ boneco),drawPoints p ]
                                                            where i=fromIntegral x 
                                                                  j=fromIntegral y 
                                                                  boneco = if (t <25 )||(t>50 && t <75) then Translate 0 15 $ (head images) else Translate 0 15 $ (head (tail images ))
drawstate _ = return $ Blank

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p |p<100 = Translate 270 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |p>100 && p<1000 = Translate 260 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |otherwise = Translate 250 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) 

velplayer::[(Terreno,[Obstaculo])]->Int->Float
velplayer ((te,obs):xs) 0 = vel te 
                where vel (Rio v) = fromIntegral v
                      vel (Estrada v) = 0 
                      vel Relva = 0
velplayer (x:xs) n = velplayer xs (n-1)
             

mapa1::Mapa->Float->Float->[(Terreno,Float,Float)]
mapa1 (Mapa lar []) _ _ = []
mapa1 (Mapa lar ((te,obs):tf)) a t= (te,a,t):mapa1 (Mapa lar tf) (a-100) t

mapa2::[(Terreno,Float,Float)]->Picture
mapa2 l = Pictures (map f l) 
                         where f (te,a,t) = Translate 0 (a-t) $ (lfundo te) 

mapa3::Mapa->Float->Float->Float->Images->[(Terreno,Obstaculo,Float,Float,Images)]
mapa3 (Mapa lar []) _ _ _ _ = []
mapa3 (Mapa lar ((te,[]):tf)) l a t textures = mapa3 (Mapa lar (tf)) (-300) (a-100) t textures
mapa3 (Mapa lar ((te,(ob1:obs)):tf)) l a t textures = (te,ob1,l+((vel te)*t),a-t,textures):mapa3 (Mapa lar ((te,obs):tf)) (l+100) a t textures 
                                                                    where vel (Rio v) = fromIntegral v
                                                                          vel (Estrada v) = fromIntegral v 
                                                                          vel Relva = 0 

mapa4::[(Terreno,Obstaculo,Float,Float,Images)]->Picture
mapa4 l = Pictures (map f l) 
              where f (te,obs,l,a,textures) = Translate l a $ (obj te obs textures) 

obj::Terreno->Obstaculo->Images->Picture  
obj (Rio v) Tronco images = Translate 0 (-100) $ last (init (init (init images)))  
obj (Estrada v) Carro images= if v<0 then  Translate 0 (-30) $ last (init images) else Translate 20 0 $ last images
obj (Relva) Arvore images = Translate 10 (-20) $ last (init (init images)) 
obj _ _ images = Blank 

cinza= makeColor 0 0 0 40

lfundo::Terreno->Picture
lfundo (Rio v)= color blue $ rectangleSolid 700 100
lfundo (Estrada v)= color cinza $ rectangleSolid 700 100
lfundo (Relva)= color green $ rectangleSolid 700 100


event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,jog,i,n,p) = return $ (Pause Resume, jogo ,jog,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (ModoJogo , jogo,jog,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (Pause Quit, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,jog,i,n,p) =return $  (Pause Resume, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,jog,i,n,p) = return $ (Opcoes Play,jogo,jog,i,n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,_,i,n,p) =return $  (Opcoes Play,jogoinit,Parado,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,_,i,n,p) = return $ (Opcoes Play,jogoinit ,Parado,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), (Move Cima),i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Baixo),i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Esquerda),i,n,p) 
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Direita) ,i,n,p) 
event _ w = return w



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,99,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo,atualizavelocidades (deslizaJogo (p+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) jog)) (p+x+y),Parado,i, 0,p+1)  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) ,jog,i,t+1,p+1)
time f w = return $ w


atualizavelocidades::Jogo->Int->Jogo
atualizavelocidades (Jogo x (Mapa l ((Rio v1,ob1):(Rio v2,ob2):resto))) _ |v2<0 = (Jogo x (Mapa l ((Rio (1),ob1):(Rio v2,ob2):resto)))
                                                                          |otherwise = (Jogo x (Mapa l ((Rio (1),ob1):(Rio v2,ob2):resto)))
atualizavelocidades (Jogo x (Mapa l ((Rio v,ob1):resto))) seed = (Jogo x (Mapa l ((Rio (velocidadealeatoria seed),ob1):resto)))
atualizavelocidades (Jogo x (Mapa l ((Estrada v1,ob1):(Estrada v2,ob2):resto))) _ |v2<0 = (Jogo x (Mapa l ((Estrada 1,ob1):(Estrada v2,ob2):resto)))
                                                                                  |otherwise = (Jogo x (Mapa l ((Estrada (-1),ob1):(Estrada v2,ob2):resto)))
atualizavelocidades (Jogo x (Mapa l ((Estrada v,ob1):resto))) seed = (Jogo x (Mapa l  ((Estrada (velocidadealeatoria seed ),ob1):resto)))
atualizavelocidades w _ = w 


velocidadealeatoria::Int->Int 
velocidadealeatoria seed |mod 2 seed == 0 = 1
                         |otherwise= (-1) 






main :: IO ()
main = do
 bonecoesq <- loadBMP "player1.bmp"
 bonecodir <- loadBMP "player2.bmp"
 log <- loadBMP "tronco.bmp"
 tree <- loadBMP "arvore1.bmp"
 car1 <- loadBMP "carro.bmp"
 car2 <- loadBMP "carro3.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (0.6) (0.6) log,scale 2 2 tree,scale (0.8) (1.2)  car1,scale (0.5) (0.8)  car2]
 playIO window white  fr (initialState images) drawState event time
-}
{-
data Opção1 = Play
            |Save
            |Sair

data Opção2 = Resume 
            |Quit
           

data Menu = Opcoes Opção1 
          |ModoJogo
          |PerdeuJogo
          |Pause Opção2

type Pontuação = Int 

type Time = Float 

type Images = [Picture]

type World = (Menu,Jogo,Jogada,Images,Time,Pontuação)

window :: Display 
window = InWindow "CrossyRoad: The Indie Game" (700, 700) (0,0)

fr :: Int
fr = 120



initialState :: Images ->World
initialState images = (Opcoes Play,jogoinit,Parado,images, 0,0)

jogoinit::Jogo
jogoinit = ( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio (1),[Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Tronco]), 
    (Estrada (-1),[Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Estrada 2,[Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 2,[Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]), 
    (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
    (Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, _, images, n,p) =return $ Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "TAKE THE L"                                                                                                                                                                           --desenha o estado perdeujogo
drawState (Pause Resume ,jogo,_, images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],Translate 300 250 $ scale (0.4) (0.4) $ drawOption "QUIT"]           --desenha o estado pause
drawState (Pause Quit ,jogo,_,images,n,p)= return $ Pictures [ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 300 250 $ scale (0.5) (0.5) $ drawOption "QUIT"]  -- desenha o estado pause 
drawState (Opcoes Play, jogo,_, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "PLAY", Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                     --desenha o menu das opçoes para jogar normal
drawState (Opcoes Save, jogo,_, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "PLAY",Color blue $ Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo,_, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "PLAY",Translate (-110) (-70) $ drawOption "SAVE", Color blue $ Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa lar l)),_,images,t,p)= return $ Pictures [(mapa2 (mapa1 (Mapa lar l) 400 t)),(mapa4 (mapa3 (Mapa lar l) (-300) 400 t images)),(Translate ((i*100)-300+((velplayer l y)*t)) (400 -(j*100)-(t)) $ boneco),drawPoints p ]
                                                            where i=fromIntegral x 
                                                                  j=fromIntegral y 
                                                                  boneco = if (t <25 )||(t>50 && t <75) then Translate 0 15 $ (head images) else Translate 0 15 $ (head (tail images ))
drawstate _ = return $ Blank

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p |p<100 = Translate 270 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |p>100 && p<1000 = Translate 260 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) 
             |otherwise = Translate 250 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) 

velplayer::[(Terreno,[Obstaculo])]->Int->Float
velplayer ((te,obs):xs) 0 = vel te 
                where vel (Rio v) = fromIntegral v
                      vel (Estrada v) = 0 
                      vel Relva = 0
velplayer (x:xs) n = velplayer xs (n-1)
             

mapa1::Mapa->Float->Float->[(Terreno,Float,Float)]
mapa1 (Mapa lar []) _ _ = []
mapa1 (Mapa lar ((te,obs):tf)) a t= (te,a,t):mapa1 (Mapa lar tf) (a-100) t

mapa2::[(Terreno,Float,Float)]->Picture
mapa2 l = Pictures (map f l) 
                         where f (te,a,t) = Translate 0 (a-t) $ (lfundo te) 

mapa3::Mapa->Float->Float->Float->Images->[(Terreno,Obstaculo,Float,Float,Images)]
mapa3 (Mapa lar []) _ _ _ _ = []
mapa3 (Mapa lar ((te,[]):tf)) l a t textures = mapa3 (Mapa lar (tf)) (-300) (a-100) t textures
mapa3 (Mapa lar ((te,(ob1:obs)):tf)) l a t textures = (te,ob1,l+((vel te)*t),a-t,textures):mapa3 (Mapa lar ((te,obs):tf)) (l+100) a t textures 
                                                                    where vel (Rio v) = fromIntegral v
                                                                          vel (Estrada v) = fromIntegral v 
                                                                          vel Relva = 0 

mapa4::[(Terreno,Obstaculo,Float,Float,Images)]->Picture
mapa4 l = Pictures (map f l) 
              where f (te,obs,l,a,textures) = Translate l a $ (obj te obs textures) 

obj::Terreno->Obstaculo->Images->Picture  
obj (Rio v) Tronco images = Translate 0 (-100) $ last (init (init (init images)))  
obj (Estrada v) Carro images= if v<0 then  Translate 0 (-30) $ last (init images) else Translate 20 0 $ last images
obj (Relva) Arvore images = Translate 10 (-20) $ last (init (init images)) 
obj _ _ images = Blank 

cinza= makeColor 0 0 0 40

lfundo::Terreno->Picture
lfundo (Rio v)= color blue $ rectangleSolid 700 100
lfundo (Estrada v)= color cinza $ rectangleSolid 700 100
lfundo (Relva)= color green $ rectangleSolid 700 100


event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,jog,i,n,p) = return $ (Pause Resume, jogo ,jog,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (ModoJogo , jogo,jog,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (Pause Quit, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,jog,i,n,p) =return $  (Pause Resume, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,jog,i,n,p) = return $ (Opcoes Play,jogo,jog,i,n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,_,i,n,p) =return $  (Opcoes Play,jogoinit,Parado,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,_,i,n,p) = return $ (Opcoes Play,jogoinit ,Parado,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), (Move Cima),i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Baixo),i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Esquerda),i,n,p) 
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Direita) ,i,n,p) 
event _ w = return w



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,99,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo,(deslizaJogo (p+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) jog)),Parado,i, 0,p+1)  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) ,jog,i,t+1,p+1)
time f w = return $ w







main :: IO ()
main = do
 bonecoesq <- loadBMP "player1.bmp"
 bonecodir <- loadBMP "player2.bmp"
 log <- loadBMP "tronco.bmp"
 tree <- loadBMP "arvore1.bmp"
 car1 <- loadBMP "carro.bmp"
 car2 <- loadBMP "carro3.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (0.6) (0.6) log,scale 2 2 tree,scale (0.8) (1.2)  car1,scale (0.5) (0.8)  car2]
 playIO window white  fr (initialState images) drawState event time
-} 
{-



data Opção1 = Play
            |Save
            |Sair

data Opção2 = Resume 
            |Quit
           

data Menu = Opcoes Opção1 
          |ModoJogo
          |PerdeuJogo
          |Pause Opção2

type Pontuação = Int 

type Time = Float 

type Images = [Picture]

type World = (Menu,Jogo,Jogada,Images,Time,Pontuação)

window :: Display 
window = InWindow "CrossyRoad" (700, 700) (0,0)

fr :: Int
fr = 120



initialState :: Images ->World
initialState images = (Opcoes Play,jogoinit,Parado,images, 0,0)

jogoinit::Jogo
jogoinit = ( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio (1),[Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]), 
    (Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore]), 
    (Estrada 1,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 2,[Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]), 
    (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
    (Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, _, images, n,p) =return $ Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "TAKE THE L"                                                                                                                                                                           --desenha o estado perdeujogo
drawState (Pause Resume ,jogo,_, images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],Translate 300 250 $ scale (0.4) (0.4) $ drawOption "QUIT"]           --desenha o estado pause
drawState (Pause Quit ,jogo,_,images,n,p)= return $ Pictures [ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 300 250 $ scale (0.5) (0.5) $ drawOption "QUIT"]  -- desenha o estado pause 
drawState (Opcoes Play, jogo,_, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "PLAY", Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                     --desenha o menu das opçoes para jogar normal
drawState (Opcoes Save, jogo,_, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "PLAY",Color blue $ Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo,_, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "PLAY",Translate (-110) (-70) $ drawOption "SAVE", Color blue $ Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l [(tf,[p1,p2,p3,p4,p5,p6,p7]),(t1,[p8,p9,p10,p11,p12,p13,p14]),(t2,[p15,p16,p17,p18,p19,p20,p21]),(t3,[p22,p23,p24,p25,p26,p27,p28]),(t4,[p29,p30,p31,p32,p33,p34,p35]),(t5,[p36,p37,p38,p39,p40,p41,p42]),(t6,[p43,p44,p45,p46,p47,p48,p49]),(t7,[p50,p51,p52,p53,p54,p55,p56])])),_, i,t,p) = 
    return $ Pictures $ [Translate 0 (400-(t)) $ lfundo tf ,
        Translate 0 (300-(t))       $ lfundo t1,Translate 0 (200-(t))      $ lfundo t2, Translate 0 (100-(t))   $ lfundo t3,Translate 0 (0-(t)) $ lfundo t4, 
        Translate 0 (-100-(t))      $ lfundo t5,Translate 0 (-200-(t))     $ lfundo t6, Translate 0 (-300-(t))  $ lfundo t7,
        Translate (-300+((vel tf)*t)) (400-(t))  $ obj tf p1  i,Translate (-200+((vel tf)*(t))) (400-(t))  $ obj tf p2  i,Translate (-100+((vel tf)*(t))) (400-(t)) $ obj tf p3 i,  Translate (0+((vel tf)*(t))) (400-(t))  $ obj tf p4 i, Translate (100+((vel tf)*(t))) (400-(t))  $ obj tf p5 i, Translate (200+((vel tf)*(t))) (400-(t))  $ obj tf p6 i, Translate (300+((vel tf)*(t))) (400-(t))  $ obj tf p7 i,
        Translate (-300+((vel t1)*t)) (300-(t))  $ obj t1 p8  i,Translate (-200+((vel t1)*(t))) (300-(t))  $ obj t1 p9  i,Translate (-100+((vel t1)*(t))) (300-(t)) $ obj t1 p10 i, Translate (0+((vel t1)*(t))) (300-(t))  $ obj t1 p11 i,Translate (100+((vel t1)*(t)))  (300-(t)) $ obj t1 p12 i,Translate (200+((vel t1)*(t))) (300-(t))  $ obj t1 p13 i,Translate (300+((vel t1)*(t))) (300-(t))  $ obj t1 p14 i,
        Translate (-300+((vel t2)*t)) (200-(t))  $ obj t2 p15 i,Translate (-200+((vel t2)*(t))) (200-(t))  $ obj t2 p16 i,Translate (-100+((vel t2)*(t))) (200-(t)) $ obj t2 p17 i, Translate (0+((vel t2)*(t))) (200-(t))  $ obj t2 p18 i,Translate (100+((vel t2)*(t))) (200-(t))  $ obj t2 p19 i,Translate (200+((vel t2)*(t))) (200-(t))  $ obj t2 p20 i,Translate (300+((vel t2)*(t))) (200-(t))  $ obj t2 p21 i,
        Translate (-300+((vel t3)*t)) (100-(t))  $ obj t3 p22 i,Translate (-200+((vel t3)*(t))) (100-(t))  $ obj t3 p23 i,Translate (-100+((vel t3)*(t))) (100-(t)) $ obj t3 p24 i, Translate (0+((vel t3)*(t))) (100-(t))  $ obj t3 p25 i,Translate (100+((vel t3)*(t))) (100-(t))  $ obj t3 p26 i,Translate (200+((vel t3)*(t))) (100-(t))  $ obj t3 p27 i,Translate (300+((vel t3)*(t))) (100-(t))  $ obj t3 p28 i,
        Translate (-300+((vel t4)*t))   (0-(t))  $ obj t4 p29 i,Translate (-200+((vel t4)*(t)))   (0-(t))  $ obj t4 p30 i,Translate (-100+((vel t4)*(t)))   (0-(t)) $ obj t4 p31 i, Translate (0+((vel t4)*(t)))   (0-(t))  $ obj t4 p32 i,Translate (100+((vel t4)*(t)))   (0-(t))  $ obj t4 p33 i,Translate (200+((vel t4)*(t))) (  0-(t))  $ obj t4 p34 i,Translate (300+((vel t4)*(t))) ( 0-(t))   $ obj t4 p35 i,
        Translate (-300+((vel t5)*t)) (-100-(t)) $ obj t5 p36 i,Translate (-200+((vel t5)*(t))) (-100-(t)) $ obj t5 p37 i,Translate (-100+((vel t5)*(t))) (-100-(t)) $ obj t5 p38 i,Translate (0+((vel t5)*(t))) (-100-(t)) $ obj t5 p39 i,Translate (100+((vel t5)*(t))) (-100-(t)) $ obj t5 p40 i,Translate (200+((vel t5)*(t))) (-100-(t)) $ obj t5 p41 i,Translate (300+((vel t5)*(t))) (-100-(t)) $ obj t5 p42 i,
        Translate (-300+((vel t6)*t)) (-200-(t)) $ obj t6 p43 i,Translate (-200+((vel t6)*(t))) (-200-(t)) $ obj t6 p44 i,Translate (-100+((vel t6)*(t))) (-200-(t)) $ obj t6 p45 i,Translate (0+((vel t6)*(t))) (-200-(t)) $ obj t6 p46 i,Translate (100+((vel t6)*(t))) (-200-(t)) $ obj t6 p47 i,Translate (200+((vel t6)*(t))) (-200-(t)) $ obj t6 p48 i,Translate (300+((vel t6)*(t))) (-200-(t)) $ obj t6 p49 i,
        Translate (-300+((vel t7)*t)) (-300-(t)) $ obj t7 p50 i,Translate (-200+((vel t7)*(t))) (-300-(t)) $ obj t7 p51 i,Translate (-100+((vel t7)*(t))) (-300-(t)) $ obj t7 p52 i,Translate (0+((vel t7)*(t))) (-300-(t)) $ obj t7 p53 i,Translate (100+((vel t7)*(t))) (-300-(t)) $ obj t7 p54 i,Translate (200+((vel t7)*(t))) (-300-(t)) $ obj t7 p55 i,Translate (300+((vel t7)*(t))) (-300-(t)) $ obj t7 p56 i,
        Translate ((k*100)-300+ (fromIntegral (aux (ty y)))*(t)) (400 -(j*100)-(t)) $ boneco, drawPoints p]                                                                                           --desenha o mapa de jogo
     where
        k=fromIntegral x
        j=fromIntegral y
        boneco = if (t <25 )||(t>50 && t <75) then (head i) else (head (tail i ))
        vel (Rio v) = l where l =fromIntegral v
        vel (Estrada v) = l where l =fromIntegral v
        vel Relva = 0
        ty 0 = tf
        ty 1= t1
        ty 2= t2
        ty 3= t3
        ty 4= t4
        ty 5= t5
        ty 6= t6 
        ty 7= t7
        ty _= t7 
        aux (Rio v )= v
        aux _ = 0
drawstate _ = return $ rectangleSolid 700 700

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p = Translate 270 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) --where l = truncate p 


obj::Terreno->Obstaculo->Images->Picture
obj (Rio v) Tronco images = Translate 0 (-100) $ last (init (init images))--color black $ rectangleSolid 90 70 
obj (Estrada v) Carro images=  Translate 0 (-30) $ last images--color white $ rectangleSolid 80 80 
obj (Relva) Arvore images = Translate 10 (-30) $ last (init images)--color brown $ circleSolid 45
obj _ _ _ = Blank 

brown= makeColor 60 30 10 60 
cinza= makeColor 0 0 0 40



river::Picture
river= color blue $ rectangleSolid 700 100

road::Picture
road= color cinza $ rectangleSolid 700 100

grass::Picture
grass = color green $ rectangleSolid 700 100



lfundo::Terreno->Picture
lfundo (Rio v)= river
lfundo (Estrada v)= road
lfundo (Relva)= grass

    
event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,jog,i,n,p) = return $ (Pause Resume, jogo ,jog,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (ModoJogo , jogo,jog,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (Pause Quit, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,jog,i,n,p) =return $  (Pause Resume, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,jog,i,n,p) = return $ (Opcoes Play,jogo,jog,i,n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,_,i,n,p) =return $  (Opcoes Play,jogoinit,Parado,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,_,i,n,p) = return $ (Opcoes Play,jogoinit ,Parado,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), (Move Cima),i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Baixo),i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Esquerda),i,n,p) 
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Direita) ,i,n,p) 
event _ w = return w



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,99,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo,(deslizaJogo (p+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) jog)),Parado,i, 0,p+1)  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) ,jog,i,t+1,p+1)
time f w = return $ w







main :: IO ()
main = do
 bonecoesq <- loadBMP "player1.bmp"
 bonecodir <- loadBMP "player2.bmp"
 log <- loadBMP "tronco.bmp"
 tree <- loadBMP "arvore1.bmp"
 car1 <- loadBMP "carro.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (0.5) (0.5) log,scale (2) (2) tree,scale (0.9) (0.9)  car1]
 playIO window white  fr (initialState images) drawState event time
-}


{-

data Opção1 = Play
            |Save
            |Sair

data Opção2 = Resume 
            |Quit
           

data Menu = Opcoes Opção1 
          |ModoJogo
          |PerdeuJogo
          |Pause Opção2

type Pontuação = Int 

type Time = Float 

type Images = [Picture]

type World = (Menu,Jogo,Jogada,Images,Time,Pontuação)

window :: Display 
window = InWindow "CrossyRoad" (700, 700) (0,0)

fr :: Int
fr = 120



initialState :: Images ->World
initialState images = (Opcoes Play,jogoinit,Parado,images, 0,0)

jogoinit::Jogo
jogoinit = ( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio (1),[Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]), 
    (Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore]), 
    (Estrada 1,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 2,[Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]), 
    (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
    (Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])]))


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, _, images, n,p) =return $ Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "TAKE THE L"                                                                                                                                                                           --desenha o estado perdeujogo
drawState (Pause Resume ,jogo,_, images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],Translate 300 250 $ scale (0.4) (0.4) $ drawOption "QUIT"]           --desenha o estado pause
drawState (Pause Quit ,jogo,_,images,n,p)= return $ Pictures [ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 300 250 $ scale (0.5) (0.5) $ drawOption "QUIT"]  -- desenha o estado pause 
drawState (Opcoes Play, jogo,_, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "PLAY", Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                     --desenha o menu das opçoes para jogar normal
drawState (Opcoes Save, jogo,_, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "PLAY",Color blue $ Translate (-110) (-70) $ drawOption "SAVE",Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo,_, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "PLAY",Translate (-110) (-70) $ drawOption "SAVE", Color blue $ Translate (-110) (-140) $ drawOption "QUIT"]                                                                                       --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l [(tf,[p1,p2,p3,p4,p5,p6,p7]),(t1,[p8,p9,p10,p11,p12,p13,p14]),(t2,[p15,p16,p17,p18,p19,p20,p21]),(t3,[p22,p23,p24,p25,p26,p27,p28]),(t4,[p29,p30,p31,p32,p33,p34,p35]),(t5,[p36,p37,p38,p39,p40,p41,p42]),(t6,[p43,p44,p45,p46,p47,p48,p49]),(t7,[p50,p51,p52,p53,p54,p55,p56])])),_, i,t,p) = 
    return $ Pictures $ [Translate 0 (400-(t)) $ lfundo tf ,
        Translate 0 (300-(t))       $ lfundo t1,Translate 0 (200-(t))      $ lfundo t2, Translate 0 (100-(t))   $ lfundo t3,Translate 0 (0-(t)) $ lfundo t4, 
        Translate 0 (-100-(t))      $ lfundo t5,Translate 0 (-200-(t))     $ lfundo t6, Translate 0 (-300-(t))  $ lfundo t7,
        Translate (-300+((vel tf)*t)) (400-(t))  $ obj tf p1  i,Translate (-200+((vel tf)*(t))) (400-(t))  $ obj tf p2  i,Translate (-100+((vel tf)*(t))) (400-(t)) $ obj tf p3 i,  Translate (0+((vel tf)*(t))) (400-(t))  $ obj tf p4 i, Translate (100+((vel tf)*(t))) (400-(t))  $ obj tf p5 i, Translate (200+((vel tf)*(t))) (400-(t))  $ obj tf p6 i, Translate (300+((vel tf)*(t))) (400-(t))  $ obj tf p7 i,
        Translate (-300+((vel t1)*t)) (300-(t))  $ obj t1 p8  i,Translate (-200+((vel t1)*(t))) (300-(t))  $ obj t1 p9  i,Translate (-100+((vel t1)*(t))) (300-(t)) $ obj t1 p10 i, Translate (0+((vel t1)*(t))) (300-(t))  $ obj t1 p11 i,Translate (100+((vel t1)*(t)))  (300-(t)) $ obj t1 p12 i,Translate (200+((vel t1)*(t))) (300-(t))  $ obj t1 p13 i,Translate (300+((vel t1)*(t))) (300-(t))  $ obj t1 p14 i,
        Translate (-300+((vel t2)*t)) (200-(t))  $ obj t2 p15 i,Translate (-200+((vel t2)*(t))) (200-(t))  $ obj t2 p16 i,Translate (-100+((vel t2)*(t))) (200-(t)) $ obj t2 p17 i, Translate (0+((vel t2)*(t))) (200-(t))  $ obj t2 p18 i,Translate (100+((vel t2)*(t))) (200-(t))  $ obj t2 p19 i,Translate (200+((vel t2)*(t))) (200-(t))  $ obj t2 p20 i,Translate (300+((vel t2)*(t))) (200-(t))  $ obj t2 p21 i,
        Translate (-300+((vel t3)*t)) (100-(t))  $ obj t3 p22 i,Translate (-200+((vel t3)*(t))) (100-(t))  $ obj t3 p23 i,Translate (-100+((vel t3)*(t))) (100-(t)) $ obj t3 p24 i, Translate (0+((vel t3)*(t))) (100-(t))  $ obj t3 p25 i,Translate (100+((vel t3)*(t))) (100-(t))  $ obj t3 p26 i,Translate (200+((vel t3)*(t))) (100-(t))  $ obj t3 p27 i,Translate (300+((vel t3)*(t))) (100-(t))  $ obj t3 p28 i,
        Translate (-300+((vel t4)*t))   (0-(t))  $ obj t4 p29 i,Translate (-200+((vel t4)*(t)))   (0-(t))  $ obj t4 p30 i,Translate (-100+((vel t4)*(t)))   (0-(t)) $ obj t4 p31 i, Translate (0+((vel t4)*(t)))   (0-(t))  $ obj t4 p32 i,Translate (100+((vel t4)*(t)))   (0-(t))  $ obj t4 p33 i,Translate (200+((vel t4)*(t))) (  0-(t))  $ obj t4 p34 i,Translate (300+((vel t4)*(t))) ( 0-(t))   $ obj t4 p35 i,
        Translate (-300+((vel t5)*t)) (-100-(t)) $ obj t5 p36 i,Translate (-200+((vel t5)*(t))) (-100-(t)) $ obj t5 p37 i,Translate (-100+((vel t5)*(t))) (-100-(t)) $ obj t5 p38 i,Translate (0+((vel t5)*(t))) (-100-(t)) $ obj t5 p39 i,Translate (100+((vel t5)*(t))) (-100-(t)) $ obj t5 p40 i,Translate (200+((vel t5)*(t))) (-100-(t)) $ obj t5 p41 i,Translate (300+((vel t5)*(t))) (-100-(t)) $ obj t5 p42 i,
        Translate (-300+((vel t6)*t)) (-200-(t)) $ obj t6 p43 i,Translate (-200+((vel t6)*(t))) (-200-(t)) $ obj t6 p44 i,Translate (-100+((vel t6)*(t))) (-200-(t)) $ obj t6 p45 i,Translate (0+((vel t6)*(t))) (-200-(t)) $ obj t6 p46 i,Translate (100+((vel t6)*(t))) (-200-(t)) $ obj t6 p47 i,Translate (200+((vel t6)*(t))) (-200-(t)) $ obj t6 p48 i,Translate (300+((vel t6)*(t))) (-200-(t)) $ obj t6 p49 i,
        Translate (-300+((vel t7)*t)) (-300-(t)) $ obj t7 p50 i,Translate (-200+((vel t7)*(t))) (-300-(t)) $ obj t7 p51 i,Translate (-100+((vel t7)*(t))) (-300-(t)) $ obj t7 p52 i,Translate (0+((vel t7)*(t))) (-300-(t)) $ obj t7 p53 i,Translate (100+((vel t7)*(t))) (-300-(t)) $ obj t7 p54 i,Translate (200+((vel t7)*(t))) (-300-(t)) $ obj t7 p55 i,Translate (300+((vel t7)*(t))) (-300-(t)) $ obj t7 p56 i,
        Translate ((k*100)-300+ (fromIntegral (aux (ty y)))*(t)) (400 -(j*100)-(t)) $ boneco, drawPoints p]                                                                                           --desenha o mapa de jogo
     where
        k=fromIntegral x
        j=fromIntegral y
        boneco = if (t <25 )||(t>50 && t <75) then (head i) else (head (tail i))
        vel (Rio v) = l where l =fromIntegral v
        vel (Estrada v) = l where l =fromIntegral v
        vel Relva = 0
        ty 0 = tf
        ty 1= t1
        ty 2= t2
        ty 3= t3
        ty 4= t4
        ty 5= t5
        ty 6= t6 
        ty 7= t7
        ty _= t7 
        aux (Rio v )= v
        aux _ = 0
drawstate _ = return $ rectangleSolid 700 700

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p = Translate 270 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) --where l = truncate p 


obj::Terreno->Obstaculo->Images->Picture
obj (Rio v) Nenhum images= Blank --color blue $ rectangleSolid 100 100 
obj (Rio v) Tronco images = head (tail (tail images))--color black $ rectangleSolid 90 70 
obj (Estrada v) Nenhum images= Blank -- color cinza $ rectangleSolid 100 100 
obj (Estrada v) Carro images=  last images--color white $ rectangleSolid 80 80 
obj (Relva) Nenhum images = Blank -- color green $ rectangleSolid 100 100
obj (Relva) Arvore images = last (init (init images))--color brown $ circleSolid 45
obj t o images = Blank 

--brown= makeColor 60 30 10 60 
cinza= makeColor 0 0 0 40



river::Picture
river= color blue $ rectangleSolid 700 100

road::Picture
road= color cinza $ rectangleSolid 700 100

grass::Picture
grass = color green $ rectangleSolid 700 100



lfundo::Terreno->Picture
lfundo (Rio v)= river
lfundo (Estrada v)= road
lfundo (Relva)= grass

    
event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (ModoJogo, jogo,jog,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = return $ (Opcoes Save, jogo,jog,i,n,p)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Save, jogo,jog,i,n,p) = return $ (Opcoes Sair, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = return $ (Opcoes Play, jogo,jog,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,jog,i,n,p) = return $ (Pause Resume, jogo ,jog,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (ModoJogo , jogo,jog,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Resume,jogo,jog,i,n,p) =return $  (Pause Quit, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,jog,i,n,p) =return $  (Pause Resume, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,jog,i,n,p) = return $ (Opcoes Play,jogo,jog,i,n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,_,i,n,p) =return $  (Opcoes Play,jogoinit,Parado,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,_,i,n,p) = return $ (Opcoes Play,jogoinit ,Parado,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), (Move Cima),i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) = 
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Baixo),i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Esquerda),i,n,p) 
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) =
     return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Direita) ,i,n,p) 
event _ w = return w



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,99,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo,(deslizaJogo (p+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) jog)),Parado,i, 0,p+1)  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) ,jog,i,t+1,p+1)
time f w = return $ w







main :: IO ()
main = do
 bonecoesq <- loadBMP "player1.bmp"
 bonecodir <- loadBMP "player2.bmp"
 log <- loadBMP "logex2.bmp"
 tree <- loadBMP "treeex2.bmp"
 car1 <- loadBMP "car1.bmp"
 car2 <- loadBMP "car2.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (0.1) (0.1) log,scale (0.1) (0.1) tree, scale (0.1) (0.1) car1,scale (0.1) (0.1) car2]
 playIO window white  fr (initialState images) drawState event time

-}
{-}
data Opcao = Normal
            |Natal
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | PerdeuJogo
          | Pause Escolha

data Escolha = Retomar
             |Quit

type World = (Menu, Jogo,Images, Time,Pontuação)
type Pontuação = Float 
type Time = Float

type Images = [Picture]

window :: Display
window = InWindow "CrossyRoad" (700, 700) (0,0)

fr :: Int
fr = 50




initialState :: Images ->World
initialState images = (Opcoes Normal,( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio (4),[Tronco,Tronco,Tronco,Tronco,Tronco,Tronco,Tronco]), 
    (Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore]), 
    (Estrada 1,[Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 2,[Nenhum,Tronco,Nenhum,Tronco,Tronco,Tronco,Tronco]), 
    (Rio (-1),[Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]),
    (Relva,[Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore])])),images, 0,0)


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, images, n,p) =return $ Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "Take the L"                                                                                                                                                                           --desenha o estado perdeujogo
drawState (Pause Retomar ,jogo,images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],Translate 300 250 $ scale (0.4) (0.4) $ drawOption "Sair"]           --desenha o estado pause
drawState (Pause Quit ,jogo,images,n,p)= return $ Pictures [ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)], Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 300 250 $ scale (0.5) (0.5) $ drawOption "Sair"]  -- desenha o estado pause 
drawState (Opcoes Normal, jogo, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "Normal", Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]                                                                                     --desenha o menu das opçoes para jogar normal
drawState (Opcoes Natal, jogo, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "Normal",Color blue $ Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]                                                                                       --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "Normal",Translate (-110) (-70) $ drawOption "Natal", Color blue $ Translate (-110) (-140) $ drawOption "Sair"]                                                                                       --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l [(tf,[p1,p2,p3,p4,p5,p6,p7]),(t1,[p8,p9,p10,p11,p12,p13,p14]),(t2,[p15,p16,p17,p18,p19,p20,p21]),(t3,[p22,p23,p24,p25,p26,p27,p28]),(t4,[p29,p30,p31,p32,p33,p34,p35]),(t5,[p36,p37,p38,p39,p40,p41,p42]),(t6,[p43,p44,p45,p46,p47,p48,p49]),(t7,[p50,p51,p52,p53,p54,p55,p56])])), images,t,p) = 
    return $ Pictures $ [Translate 0 (400-(2*t/5)) $ lfundo tf ,
        Translate 0 (300-(2*t/5))       $ lfundo t1,Translate 0 (200-(2*t/5))      $ lfundo t2, Translate 0 (100-(2*t/5))   $ lfundo t3,Translate 0 (0-(2*t/5)) $ lfundo t4, 
        Translate 0 (-100-(2*t/5))      $ lfundo t5,Translate 0 (-200-(2*t/5))     $ lfundo t6, Translate 0 (-300-(2*t/5))  $ lfundo t7,
        Translate (-300+((vel tf)*(2*t/5))) (400-(2*t/5))  $ obj tf p1 , Translate (-200+((vel tf)*(2*t/5))) (400-(2*t/5))  $ obj tf p2 ,Translate (-100+((vel tf)*(2*t/5))) (400-(2*t/5)) $ obj tf p3,  Translate (0+((vel tf)*(2*t/5))) (400-(2*t/5))  $ obj tf p4, Translate (100+((vel tf)*(2*t/5))) (400-(2*t/5))  $ obj tf p5, Translate (200+((vel tf)*(2*t/5))) (400-(2*t/5))  $ obj tf p6, Translate (300+((vel tf)*(2*t/5))) (400-(2*t/5))  $ obj tf p7,
        Translate (-300+((vel t1)*(2*t/5))) (300-(2*t/5))  $ obj t1 p8 , Translate (-200+((vel t1)*(2*t/5))) (300-(2*t/5))  $ obj t1 p9 ,Translate (-100+((vel t1)*(2*t/5))) (300-(2*t/5)) $ obj t1 p10, Translate (0+((vel t1)*(2*t/5))) (300-(2*t/5))  $ obj t1 p11,Translate (100+((vel t1)*(2*t/5)))  (300-(2*t/5)) $ obj t1 p12,Translate (200+((vel t1)*(2*t/5))) (300-(2*t/5))  $ obj t1 p13,Translate (300+((vel t1)*(2*t/5))) (300-(2*t/5))  $ obj t1 p14,
        Translate (-300+((vel t2)*(2*t/5))) (200-(2*t/5))  $ obj t2 p15, Translate (-200+((vel t2)*(2*t/5))) (200-(2*t/5))  $ obj t2 p16,Translate (-100+((vel t2)*(2*t/5))) (200-(2*t/5)) $ obj t2 p17, Translate (0+((vel t2)*(2*t/5))) (200-(2*t/5))  $ obj t2 p18,Translate (100+((vel t2)*(2*t/5))) (200-(2*t/5))  $ obj t2 p19,Translate (200+((vel t2)*(2*t/5))) (200-(2*t/5))  $ obj t2 p20,Translate (300+((vel t2)*(2*t/5))) (200-(2*t/5))  $ obj t2 p21,
        Translate (-300+((vel t3)*(2*t/5))) (100-(2*t/5))  $ obj t3 p22, Translate (-200+((vel t3)*(2*t/5))) (100-(2*t/5))  $ obj t3 p23,Translate (-100+((vel t3)*(2*t/5))) (100-(2*t/5)) $ obj t3 p24, Translate (0+((vel t3)*(2*t/5))) (100-(2*t/5))  $ obj t3 p25,Translate (100+((vel t3)*(2*t/5))) (100-(2*t/5))  $ obj t3 p26,Translate (200+((vel t3)*(2*t/5))) (100-(2*t/5))  $ obj t3 p27,Translate (300+((vel t3)*(2*t/5))) (100-(2*t/5))  $ obj t3 p28,
        Translate (-300+((vel t4)*(2*t/5)))   (0-(2*t/5))  $ obj t4 p29 ,Translate (-200+((vel t4)*(2*t/5)))   (0-(2*t/5))  $ obj t4 p30,Translate (-100+((vel t4)*(2*t/5)))   (0-(2*t/5)) $ obj t4 p31, Translate (0+((vel t4)*(2*t/5)))   (0-(2*t/5))  $ obj t4 p32,Translate (100+((vel t4)*(2*t/5)))   (0-(2*t/5))  $ obj t4 p33,Translate (200+((vel t4)*(2*t/5))) (  0-(2*t/5))  $ obj t4 p34,Translate (300+((vel t4)*(2*t/5))) ( 0-(2*t/5))   $ obj t4 p35,
        Translate (-300+((vel t5)*(2*t/5))) (-100-(2*t/5)) $ obj t5 p36 ,Translate (-200+((vel t5)*(2*t/5))) (-100-(2*t/5)) $ obj t5 p37,Translate (-100+((vel t5)*(2*t/5))) (-100-(2*t/5)) $ obj t5 p38,Translate (0+((vel t5)*(2*t/5))) (-100-(2*t/5)) $ obj t5 p39,Translate (100+((vel t5)*(2*t/5))) (-100-(2*t/5)) $ obj t5 p40,Translate (200+((vel t5)*(2*t/5))) (-100-(2*t/5)) $ obj t5 p41,Translate (300+((vel t5)*(2*t/5))) (-100-(2*t/5)) $ obj t5 p42,
        Translate (-300+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p43 ,Translate (-200+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p44,Translate (-100+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p45,Translate (0+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p46,Translate (100+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p47,Translate (200+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p48,Translate (300+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p49,
        Translate (-300+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p50 ,Translate (-200+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p51,Translate (-100+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p52,Translate (0+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p53,Translate (100+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p54,Translate (200+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p55,Translate (300+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p56,
        Translate ((i*100)-300+ (fromIntegral (aux (ty y)))*(2*t/5)) (400 -(j*100)-(2*t/5)) $ boneco, drawPoints p]                                                                                           --desenha o mapa de jogo
     where
        i=fromIntegral x
        j=fromIntegral y
        boneco = if (t <25 )||(t>50 && t <75)||(t>100 && t<125 )||(t>150 && t<175 )||(t>200 &&t<225)  then (head images) else (last images)
        vel (Rio v) = l where l =fromIntegral v
        vel (Estrada v) = l where l =fromIntegral v
        vel Relva = 0
        ty 0 = tf
        ty 1= t1
        ty 2= t2
        ty 3= t3
        ty 4= t4
        ty 5= t5
        ty 6= t6 
        ty 7= t7
        ty _= t7 
        aux (Rio v )= v
        aux _ = 0
    


                             

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p = Translate 270 300 $ color red $ Scale (0.4) (0.4) $ Text (show p)





{-A função obj desenha os obstáculos
-}
obj::Terreno->Obstaculo->Picture
obj (Rio v) Nenhum = color blue $ rectangleSolid 100 100 
obj (Rio v) Tronco = color black $ rectangleSolid 90 70 
obj (Estrada v) Nenhum = color cinza $ rectangleSolid 100 100 
obj (Estrada v) Carro = color white $ rectangleSolid 80 80 
obj (Relva) Nenhum = color green $ rectangleSolid 100 100
obj (Relva) Arvore = color brown $ circleSolid 45
obj t o = color orange $ rectangleSolid 100 100

brown= makeColor 78 80 40 10 
cinza= makeColor 0 0 0 40



river::Picture
river= color blue $ rectangleSolid 700 100

road::Picture
road= color cinza $ rectangleSolid 700 100

grass::Picture
grass = color green $ rectangleSolid 700 100


     
{-A função lfundo desenha a "linha" em função do terreno
-}

lfundo::Terreno->Picture
lfundo (Rio v)= river
lfundo (Estrada v)= road
lfundo (Relva)= grass





event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Normal, jogo,i,n,p) = return $ (ModoJogo, jogo,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Natal, jogo,i,n,p) = return $ (ModoJogo, jogo,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Normal, jogo,i,n,p) = return $ (Opcoes Sair, jogo,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,i,n,p) = return $ (Opcoes Natal, jogo,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Natal, jogo,i,n,p) = return $ (Opcoes Normal, jogo,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Normal, jogo,i,n,p) = return $ (Opcoes Natal, jogo,i,n,p)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Natal, jogo,i,n,p) = return $ (Opcoes Sair, jogo,i,n,p)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,i,n,p) = return $ (Opcoes Normal, jogo,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,i,n,p) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,i,n,p) = return $ (Pause Retomar, jogo ,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Retomar,jogo,i,n,p) =return $  (ModoJogo , jogo ,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Retomar,jogo,i,n,p) =return $  (Pause Quit, jogo ,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,i,n,p) =return $  (Pause Retomar, jogo ,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,i,n,p) =
    do exitSuccess 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,i,n,p) =return $  (Opcoes Normal,( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])) ,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,i,n,p) = return $ (Opcoes Normal,( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])) ,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p)   = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo,(Jogo (Jogador (posit  ((animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Move Cima))))) (Mapa l to)),i,n,p)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo,(Jogo (Jogador (posit  ((animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Move Baixo))))) (Mapa l to)),i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p)    | jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True = return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p)  
                                                                                                        | tronco2 (Jogo (Jogador (x, y)) (Mapa l to))==True = return $  (ModoJogo,(Jogo (Jogador (x-1,y)) (Mapa l to)),i,n,p)
                                                                                                        | otherwise = return $ (ModoJogo,(Jogo (Jogador (posit  ((animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Move Esquerda))))) (Mapa l to)),i,n,p)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p)   | jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True = return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p)  
                                                                                                        | tronco2 (Jogo (Jogador (x, y)) (Mapa l to))==True = return $ (ModoJogo,(Jogo (Jogador (x+1,y)) (Mapa l to)),i,n,p) 
                                                                                                        | otherwise = return $ (ModoJogo,(Jogo (Jogador (posit  ((animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Move Direita))))) (Mapa l to)),i,n,p)
event _ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo,(Jogo(Jogador (x, y))  (Mapa l to)) ,i,n,p) 
event _ w = return w


{-a função tronco2 verifica se o jogador está em cima de um tronco
-}
tronco2::Jogo -> Bool
tronco2 (Jogo (Jogador (x,0)) (Mapa l ((te,obs):tf))) = if hatronco x obs ==True then True else False
tronco2 (Jogo (Jogador (x,y)) (Mapa l ((te,obs):tf))) = tronco2 (Jogo (Jogador (x,y-1)) (Mapa l (tf))) 

{-A função posit dá as cordenadas de um jogador
-}
posit::Jogo -> (Int,Int)
posit  (Jogo (Jogador (x,y)) (Mapa l to)) = (x,y)



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), i,249,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,0,(0.0)) else return $ (ModoJogo,(deslizaJogo (p+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Parado))),i, 0,p + (0.01))  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,0,(0.0)) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) , i,t+1,p + (0.01))
time f (m,j,i,t,p)= return $ (m,j,i,t,p)



main :: IO ()
main = do
 bonecoesq <- loadBMP "boneco_perna_es.bmp"
 bonecodir <- loadBMP "boneco_perna_di.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir]
 playIO window white  fr (initialState images) drawState event time

-}





