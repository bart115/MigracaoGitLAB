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
    (Estrada 1,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum]),
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
drawstate (ModoJogo,(Jogo (Jogador (x,y)) (Mapa lar l)),_,images,t,p)= return $ Pictures [(mapa2 (mapa1 (Mapa lar l) 400 t)),(mapa4 (mapa3 (Mapa lar l) 400 (-300) t images)),(Translate ((i*100)-300) (400 -(j*100)-(t)) $ boneco),drawPoints p ]
                                                                                                                                                                                             where i=fromIntegral x 
                                                                                                                                                                                                   j=fromIntegral y 
                                                                                                                                                                                                   boneco = if (t <25 )||(t>50 && t <75) then (head images) else (head (tail images ))
drawstate _ = return $ Blank

drawOption option = Translate (-100) 100 $ Scale (0.5) (0.5) $ Text option
drawPoints p = Translate 270 300 $ color red $ Scale (0.4) (0.4) $ Text (show p) 


mapa1::Mapa->Float->Float->[(Terreno,Float,Float)]
mapa1 (Mapa lar []) _ _ = []
mapa1 (Mapa lar ((te,obs):tf)) a t= (te,a,t):mapa1 (Mapa lar tf) (a-100) t

mapa2::[(Terreno,Float,Float)]->Picture
mapa2 l = Pictures (map f l) 
                         where f (te,a,t) = Translate 0 (a-t) $ (lfundo te) 

mapa3::Mapa->Float->Float->Float->Images->[(Terreno,Obstaculo,Float,Float,Images)]
mapa3 (Mapa lar []) _ _ _ _ = []
mapa3 (Mapa lar ((te,[]):tf)) a l t textures = mapa3 (Mapa lar (tf)) (a-100) (-300) t textures
mapa3 (Mapa lar ((te,(ob1:obs)):tf)) a l t textures = (te,ob1,a-t,l+((vel te)* t),textures):mapa3 (Mapa lar ((te,obs):tf)) a (l+100) t textures 
                                                                    where vel (Rio v) = fromIntegral v
                                                                          vel (Estrada v) = fromIntegral v 
                                                                          vel Relva = 0 

mapa4::[(Terreno,Obstaculo,Float,Float,Images)]->Picture
mapa4 l = Pictures (map f l) 
              where f (te,obs,a,l,textures) = Translate a l $ (obj te obs textures) 

obj::Terreno->Obstaculo->Images->Picture
obj (Rio v) Nenhum images= Blank --color blue $ rectangleSolid 100 100 
obj (Rio v) Tronco images = Translate 0 (-100) $ last (init (init images))
obj (Estrada v) Nenhum images= Blank -- color cinza $ rectangleSolid 100 100 
obj (Estrada v) Carro images=  Translate 0 (-50) $ last images
obj (Relva) Nenhum images = Blank -- color green $ rectangleSolid 100 100
obj (Relva) Arvore images = Translate 10 (-30) $ last (init images)
obj _ _ images = Blank 


brown= makeColor 60 30 10 60 
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
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (0.6) (0.6) log,scale (1.8) (1.8) tree,scale (0.8) (0.8)  car1]
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
        boneco = if (t <25 )||(t>50 && t <75) then (head i) else (last i )
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
obj (Rio v) Tronco images = color black $ rectangleSolid 90 70 
obj (Estrada v) Nenhum images= Blank -- color cinza $ rectangleSolid 100 100 
obj (Estrada v) Carro images=  color white $ rectangleSolid 80 80 
obj (Relva) Nenhum images = Blank -- color green $ rectangleSolid 100 100
obj (Relva) Arvore images = color brown $ circleSolid 45
obj t o images = Blank 

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
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir]
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


drawState :: World ->Picture
drawState (PerdeuJogo, jogo, _, images,n,p) =Scale (0.5) (0.5) (Translate (-300) 0 (Color red (Text "TAKE THE L")))                                                                                                                                                                          --desenha o estado perdeujogo
drawState (Pause Resume ,jogo,_, images,p)= Pictures [pause1,pause2,quitar]
                                            where pause1= color red  (Translate (-10) 0 (polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)]))
                                                  pause2= color red (Translate 20 0 (polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)])) 
                                                  quitar= Translate 300 250 (scale (0.4) (0.4) (drawOption "QUIT") ) 
drawState (Pause Quit ,jogo,_,images,n,p)   = Pictures [pause3,pause4,quitar2]
                                            where pause3= (Translate (-10) 0 (polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)]))
                                                  pause4= (Translate 20 0 (polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)])) 
                                                  quitar2= color red (Translate 300 250 (scale (0.4) (0.4) (drawOption "QUIT"))) 
drawState (Opcoes Play, jogo,_, images,n,p) = Pictures [opplay,opsave,opsair]
                                           where opplay=Color blue (Translate (-110) 0 (drawOption "PLAY"))
                                                 opsave=Translate (-110) (-70) (drawOption "SAVE")
                                                 opsair=Translate (-110) (-140) (drawOption "QUIT") 
drawState (Opcoes Save, jogo,_, images,n,p) = Pictures [opplay2,opsave2,opsair2]
                                           where opplay2=Translate (-110) 0 (drawOption "PLAY")
                                                 opsave2=color blue (Translate (-110) (-70) (drawOption "SAVE"))
                                                 opsair2=Translate (-110) (-140) (drawOption "QUIT") 
drawState (Opcoes Sair, jogo,_, images,n,p) = Pictures [opplay3,opsave3,opsair3]
                                           where opplay3=Translate (-110) 0 (drawOption "PLAY")
                                                 opsave3=Translate (-110) (-70) (drawOption "SAVE")
                                                 opsair3=color blue (Translate (-110) (-140) (drawOption "QUIT"))
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa lar to)),_,images,n,p) = fundos ++ obstaculos ++ playe ++ pontos
                                                                                     where fundos = desenhafundos1 (Mapa lar to) 400
                                                                                           obstaculos = desenhaobs (Mapa lar to) 400 images 
                                                                                           playe =desenhaplayer (Jogo (Jogador (x,y)) (Mapa lar to)) images n
                                                                                           pontos = desenhapoints p 
--    desenhafundos (Mapa lar to) 400  ++ desenhaobs (Mapa lar to) 400 textures ++ desenhaplayer (Jogo (Jogador (x,y)) (Mapa lar to)) images n++ desenhapoints p 
drawstate _ = rectangleSolid 700 700

drawOption option = Translate (-100) 100 (Scale (0.5) (0.5) (Text option))
desenhapoints p = Translate 270 300 $ color red (Scale (0.4) (0.4) (Text (show p))) --where l = truncate p 

{-
desenhafundosaux::Mapa->[Picture]
desenhafundosaux (Mapa lar to) n1 = map f to where f (Rio v,_)= river
                                                f (Estrada v,_)=road 
                                                f (Relva,_)=grass                                
-}
desenhafundos1::Mapa->Int->[Picture]
desenhafundos1 (Mapa lar []) _ = []
desenhafundos1 (Mapa lar ((Rio v,_):tf)) n1= (Translate 0 n1 river) ++ desenhafundos1 (Mapa lar (tf)) (n1-100)
desenhafundos1 (Mapa lar ((Estrada v,_):tf)) n1= (Translate 0 n1 road) ++ desenhafundos1 (Mapa lar (tf)) (n1-100)
desenhafundos1 (Mapa lar ((Relva,_):tf)) n1= (Translate 0 n1 grass) ++ desenhafundos1 (Mapa lar (tf)) (n1-100)


desenhaobs::Mapa->Int->[Picture]->[Picture]
desenhaobs (Mapa lar []) n1 textures = []  
desenhaobs (Mapa lar [(te,obs)]) n1 textures= (Translate 0 n1 (desenhalinha obs (-300)) )
desenhaobs (Mapa lar ((te,obs):t)) n1 texture = (Translate 0 n1 (desenhalinha obs (-300)) ) ++ desenhaobs (Mapa lar t) (n1-100)


desenhalinha::(Terreno,[Obstaculo])->Int->[Picture]->[Picture]
desenhalinha (_,[]) n1 textures = [] 
desenhalinha (Rio v,(Tronco:xs)) n1 images = (Translate n1 0 (head (tail (tail (images)))))++ desenhalinha (Rio v,xs) n1+100 images
desenhalinha (Rio v,(Nenhum:xs)) n1 images = desenhalinha (Rio v,xs) n1+100 images  
desenhalinha (Estrada v,(Carro:xs)) n1 images = if v >0 then (Translate n1 0 (last (init images))) ++ desenhalinha (Estrada v,xs) n1+100 images else (Translate n1 0 (last images)) ++ desenhalinha (Estrada v,xs) n1+100 images
desenhalinha (Estrada v,(Nenhum:xs)) n1 images = desenhalinha (Estrada v,xs) n1+100 images
desenhalinha (Relva,(Arvore:xs)) n1 images = (Translate n1 0 (last (init (init images)))) ++ desenhalinha (Relva,xs) n1+100 images
desenhalinha (Relva,(Nenhum:xs)) n1 images = desenhalinha (Relva,xs) n1+100 images

desenhaplayer::Jogo->Images->Float->Picture
desenhaplayer (Jogo (Jogador (x,y)) (Mapa lar to)) images t =Translate ((i*100)-300) (400 -(j*100)) boneco 
                               where i=fromIntegral x 
                                     j=fromIntegral y
                                     boneco = if (t <30 )||(t>60 && t <90) then (head images) else (head (init images))





river::Picture
river= color blue (rectangleSolid 700 100)

road::Picture
road= color black (rectangleSolid 700 100)

grass::Picture
grass = color green (rectangleSolid 700 100)




    
event :: Event -> World -> World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Play, jogo,jog,i,n,p) =  (ModoJogo, jogo,jog,i,n,p)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Save, jogo,jog,i,n,p) =  (ModoJogo, jogo,jog,i,n,p)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Play, jogo,jog,i,n,p) = (Opcoes Sair, jogo,jog,i,n,p)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) =  (Opcoes Save, jogo,jog,i,n,p)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Save, jogo,jog,i,n,p) =  (Opcoes Play, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Play, jogo,jog,i,n,p) =  (Opcoes Save, jogo,jog,i,n,p)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Save, jogo,jog,i,n,p) =  (Opcoes Sair, jogo,jog,i,n,p)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) = (Opcoes Play, jogo,jog,i,n,p)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,jog,i,n,p) =                                                         --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,jog,i,n,p) = (Pause Resume, jogo ,jog,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause Resume,jogo,jog,i,n,p) =  (ModoJogo , jogo,jog,i,n,p) 
event (EventKey (SpecialKey KeyUp) Down _ _) (Pause Resume,jogo,jog,i,n,p) = (Pause Quit, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (Pause Quit,jogo,jog,i,n,p) =  (Pause Resume, jogo ,jog,i,n,p) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (Pause Quit,jogo,jog,i,n,p) =  (Opcoes Play,jogo,jog,i,n,p)
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,_,i,n,p) =  (Opcoes Play,jogoinit,Parado,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,_,i,n,p) =  (Opcoes Play,jogoinit ,Parado,i,n,p)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p)   = 
     (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), (Move Cima),i,n,p) 
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x log <- loadBMP "logex.bmp"
 tree <- loadBMP "treeex.bmp"
 car1 <- loadBMP "car1.bmp"
 car2 <- loadBMP "car2.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (1.1) (1.1) log,scale (1.1) (1.1) tree,scale (1.1) (1.1) car1,scale (1.1) (1.1) car2]
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),_,i,n,p) =
     (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),(Move Direita) ,i,n,p) 
event _ w =  w



time :: Float -> World ->World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,99,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else (ModoJogo,(deslizaJogo (p+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) jog)),Parado,i,0,p+1)  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),jog,i,n,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),jog,i,0,0) else (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) ,jog,i,n+1,p+1)
time f w = w 







main :: IO ()
main = do
 bonecoesq <- loadBMP "player1.bmp"
 bonecodir <- loadBMP "player2.bmp"
 log <- loadBMP "logex.bmp"
 tree <- loadBMP "treeex.bmp"
 car1 <- loadBMP "car1.bmp"
 car2 <- loadBMP "car2.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir,scale (1.1) (1.1) log,scale (1.1) (1.1) tree,scale (1.1) (1.1) car1,scale (1.1) (1.1) car2]
 play window white  fr (initialState images) drawState event time

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
{-a função princpal q usei foi a playIO 
tem 50 frames por segundo
cada quadrado tem de tamanho 100 por 100
é suposto o mapa ter 700 por 700 ou seja 7 linhas e 7 colunas
o mapa inicial por enquanto é só para testes

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
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), i,249,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,0,(0.0)) else return $ (ModoJogo,(deslizaJogo (z+x+y) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Parado))),i, 0,p + (0.01))  
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,0,(0.0)) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) , i,t+1,p + (0.01))
time f (m,j,i,t,p)= return $ (m,j,i,t,p)



main :: IO ()
main = do
 bonecoesq <- loadBMP "boneco_perna_es.bmp"
 bonecodir <- loadBMP "boneco_perna_di.bmp"
 let images = [scale (1.1) (1.1) bonecoesq, scale (1.1) (1.1) bonecodir]
 playIO window white  fr (initialState images) drawState event time

-}





