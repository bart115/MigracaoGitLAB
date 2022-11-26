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




{-a função princpal q usei foi a playIO 
tem 50 frames por segundo
cada quadrado tem de tamanho 100 por 100
é suposto o mapa ter 700 por 700 ou seja 7 linhas e 7 colunas
o mapa inicial por enquanto é só para testes






-}

data Opcao = Normal
            |Natal
            | Sair

data Menu = Opcoes Opcao
          | ModoJogo 
          | PerdeuJogo
          | Pause 

type World = (Menu, Jogo,Images, Time,Pontuação)
type Pontuação = Int
type Time = Float

type Images = [Picture]

window :: Display
window = InWindow "CrossyRoad" (700, 900) (0,0)

fr :: Int
fr = 50

initialState :: Images ->World
initialState images = (Opcoes Normal,( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Arvore,Arvore,Arvore,Nenhum,Arvore,Arvore,Arvore]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Rio 1,[Nenhum,Tronco,Nenhum,Nenhum,Tronco,Nenhum,Nenhum]), 
    (Rio (-1),[Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])),images, 0,0)


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, images, n,p) =return $  Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "Take the L"                                                                                                --desenha o estado perdeujogo
drawState (Pause ,jogo,images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)]]   --desenha o estado pause
drawState (Opcoes Normal, jogo, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "Normal", Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]           --desenha o menu das opçoes para jogar normal
drawState (Opcoes Natal, jogo, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "Normal",Color blue $ Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]             --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "Normal",Translate (-110) (-70) $ drawOption "Natal", Color blue $ Translate (-110) (-140) $ drawOption "Sair"]             --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l [(tf,[p1,p2,p3,p4,p5,p6,p7]),(t1,[p8,p9,p10,p11,p12,p13,p14]),(t2,[p15,p16,p17,p18,p19,p20,p21]),(t3,[p22,p23,p24,p25,p26,p27,p28]),(t4,[p29,p30,p31,p32,p33,p34,p35]),(t5,[p36,p37,p38,p39,p40,p41,p42]),(t6,[p43,p44,p45,p46,p47,p48,p49]),(t7,[p50,p51,p52,p53,p54,p55,p56])])), images,t,p) = 
    return $ Pictures $ [Translate 0 (400-(2*t/5)) $ lfundo tf ,
        Translate 0 (300-(2*t/5))       $ lfundo t1,Translate 0 (200-(2*t/5))      $ lfundo t2, Translate 0 (100-(2*t/5))  $ lfundo t3,Translate 0 (0-(2*t/5)) $ lfundo t4, 
        Translate 0 (-100-(2*t/5))      $ lfundo t5,Translate 0 (-200-(2*t/5))     $ lfundo t6, Translate 0 (-300-(2*t/5)) $ lfundo t7,
        Translate (-300+(vel tf)/5) (400-(2*t/5))  $ obj tf p1 ,Translate (-200+(vel tf)/5) (400-(2*t/5))  $ obj tf p2 ,Translate (-100+(vel tf)/5) (400-(2*t/5)) $ obj tf p3,  Translate (0+(vel tf)/5) (400-(2*t/5))  $ obj tf p4, Translate (100+(vel tf)/5) (400-(2*t/5))  $ obj tf p5, Translate (200+(vel tf)/5) (400-(2*t/5))  $ obj tf p6, Translate (300+(vel tf)/5) (400-(2*t/5))  $ obj tf p7,
        Translate (-300+(vel t1)/5) (300-(2*t/5))  $ obj t1 p8 ,Translate (-200+(vel t1)/5) (300-(2*t/5))  $ obj t1 p9 ,Translate (-100+(vel t1)/5) (300-(2*t/5)) $ obj t1 p10, Translate (0+(vel t1)/5) (300-(2*t/5))  $ obj t1 p11,Translate (100+(vel t1)/5)  (300-(2*t/5)) $ obj t1 p12,Translate (200+(vel t1)/5) (300-(2*t/5))  $ obj t1 p13,Translate (300+(vel t1)/5) (300-(2*t/5))  $ obj t1 p14,
        Translate (-300+(vel t2)/5) (200-(2*t/5))  $ obj t2 p15,Translate (-200+(vel t2)/5) (200-(2*t/5))  $ obj t2 p16,Translate (-100+(vel t2)/5) (200-(2*t/5)) $ obj t2 p17, Translate (0+(vel t2)/5) (200-(2*t/5))  $ obj t2 p18,Translate (100+(vel t2)/5) (200-(2*t/5))  $ obj t2 p19,Translate (200+(vel t2)/5) (200-(2*t/5))  $ obj t2 p20,Translate (300+(vel t2)/5) (200-(2*t/5))  $ obj t2 p21,
        Translate (-300+(vel t3)/5) (100-(2*t/5))  $ obj t3 p22,Translate (-200+(vel t3)/5) (100-(2*t/5))  $ obj t3 p23,Translate (-100+(vel t3)/5) (100-(2*t/5)) $ obj t3 p24, Translate (0+(vel t3)/5) (100-(2*t/5))  $ obj t3 p25,Translate (100+(vel t3)/5) (100-(2*t/5))  $ obj t3 p26,Translate (200+(vel t3)/5) (100-(2*t/5))  $ obj t3 p27,Translate (300+(vel t3)/5) (100-(2*t/5))  $ obj t3 p28,
        Translate (-300+(vel t4)/5)   (0-(2*t/5))  $ obj t4 p29 ,Translate (-200+(vel t4)/5)   (0-(2*t/5)) $ obj t4 p30,Translate (-100+(vel t4)/5)   (0-(2*t/5)) $ obj t4 p31, Translate (0+(vel t4)/5)   (0-(2*t/5))  $ obj t4 p32,Translate (100+(vel t4)/5)   (0-(2*t/5))  $ obj t4 p33,Translate (200+(vel t4)/5) (  0-(2*t/5))  $ obj t4 p34,Translate (300+(vel t4)/5) ( 0-(2*t/5))   $ obj t4 p35,
        Translate (-300+(vel t5)*t/5) (-100-(2*t/5)) $ obj t5 p36 ,Translate (-200+(vel t5)/5) (-100-(2*t/5))$ obj t5 p37,Translate (-100+(vel t5)/5) (-100-(2*t/5)) $ obj t5 p38,Translate (0+(vel t5)/5) (-100-(2*t/5)) $ obj t5 p39,Translate (100+(vel t5)/5) (-100-(2*t/5)) $ obj t5 p40,Translate (200+(vel t5)/5) (-100-(2*t/5)) $ obj t5 p41,Translate (300+(vel t5)/5) (-100-(2*t/5)) $ obj t5 p42,
        Translate (-300+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p43 ,Translate (-200+(((vel t6)*(t/5)))) (-200-(2*t/5))$ obj t6 p44,Translate (-100+((vel t6)*(2*t/5))) (-200-(2*t/5)) $ obj t6 p45,Translate (0+(((vel t6)*(2*t/5)))) (-200-(2*t/5)) $ obj t6 p46,Translate (100+(((vel t6)*(2*t/5)))) (-200-(2*t/5)) $ obj t6 p47,Translate (200+(((vel t6)*(2*t/5)))) (-200-(2*t/5)) $ obj t6 p48,Translate (300+(((vel t6)*(2*t/5)))) (-200-(2*t/5)) $ obj t6 p49,
        Translate (-300+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p50 ,Translate (-200+(((vel t7)*(t/5)))) (-300-(2*t/5))$ obj t7 p51,Translate (-100+((vel t7)*(2*t/5))) (-300-(2*t/5)) $ obj t7 p52,Translate (0+(((vel t7)*(2*t/5)))) (-300-(2*t/5)) $ obj t7 p53,Translate (100+(((vel t7)*(2*t/5)))) (-300-(2*t/5)) $ obj t7 p54,Translate (200+(((vel t7)*(2*t/5)))) (-300-(2*t/5)) $ obj t7 p55,Translate (300+(((vel t7)*(2*t/5)))) (-300-(2*t/5)) $ obj t7 p56,
        Translate ((i*100)-300) (400 -(j*100)-(2*t/5)) $ boneco]                                                                                           --desenha o mapa de jogo
     where
        i=fromIntegral x
        j=fromIntegral y
        boneco = if (t <25 )||(t>50 && t <75)||(t>100 && t<125 )||(t>150 && t<175 )||(t>200 &&t<225)  then (head images) else (last images)
        vel (Rio v) = l where l =fromIntegral v
        vel (Estrada v) = l where l =fromIntegral v
        vel Relva = 0

drawOption option = Translate 0 0 $ Scale (0.5) (0.5) $ Text option

obj::Terreno->Obstaculo->Picture
obj (Rio v) Nenhum = color blue $ rectangleSolid 100 100 
obj (Rio v) Tronco = color black $ rectangleSolid 80 80 
obj (Estrada v) Nenhum = color black $ rectangleSolid 100 100 
obj (Estrada v) Carro = color white $ rectangleSolid 80 80 
obj (Relva) Nenhum = color green $ rectangleSolid 100 100
obj (Relva) Arvore = color blue $ circleSolid 30
obj t o = color orange $ rectangleSolid 100 100



river::Picture
river= color blue $ rectangleSolid 700 100

road::Picture
road= color  black $ rectangleSolid 700 100

grass::Picture
grass = color green $ rectangleSolid 700 100
     


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
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,i,n,p) =                                                       --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,i,n,p) = return $ (Pause , jogo ,i,n,p)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause ,jogo,i,n,p) =return $  (ModoJogo , jogo ,i,n,p) 
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
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo,(Jogo (Jogador (posit  ((animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Move Esquerda))))) (Mapa l to)),i,n,p)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p)= 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo,(Jogo (Jogador (posit  ((animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Move Direita))))) (Mapa l to)),i,n,p)
event _ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo,(Jogo (Jogador (posit  ((animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Parado))))) (Mapa l to)) ,i,n,p) 
event _ w = return w




posit::Jogo -> (Int,Int)
posit  (Jogo (Jogador (x,y)) (Mapa l to)) = (x,y)



time :: Float -> World ->IO World
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), i,249,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,0,0) else return $ (ModoJogo,(deslizaJogo (p+3) (animaJogo (Jogo (Jogador (x, y)) (Mapa l to)) (Parado))),i, 0,p+1)
time f (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)), i,t,p) = if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to)) ==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,0,0) else return $ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)) , i,t+1,p+1)
time f (m,j,i,t,p)= return $ (m,j,i,t,p)



main :: IO ()
main = do
 bonecoesq <- loadBMP "bonecoesq.bmp"
 bonecodir <- loadBMP "boneodir.bmp"
 let images = [scale 0.8 0.8 bonecoesq, scale 0.8 0.8 bonecodir]
 playIO window white  fr (initialState images) drawState event time








