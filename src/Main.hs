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
window = InWindow "CrossyRoad" (700, 700) (0,0)

fr :: Int
fr = 50

initialState :: Images ->World
initialState images = (Opcoes Normal,( Jogo (Jogador (3,3)) (Mapa 7 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])),images, 0,0)


drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, images, n,p) =return $  Scale (0.5) (0.5) $ Translate (-300) 0 $ Color red $ Text "Take the L"                                                                                                --desenha o estado perdeujogo
drawState (Pause ,jogo,images,n,p)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)]]   --desenha o estado pause
drawState (Opcoes Normal, jogo, images,n,p) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "Normal", Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]           --desenha o menu das opçoes para jogar normal
drawState (Opcoes Natal, jogo, images,n,p) =return $  Pictures [Translate (-110) 0 $ drawOption "Normal",Color blue $ Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]             --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo, images,n,p) = return $ Pictures [Translate (-110) 0 $ drawOption "Normal",Translate (-110) (-70) $ drawOption "Natal", Color blue $ Translate (-110) (-140) $ drawOption "Sair"]             --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l [tf,t1,t2,t3,t4,t5,t6,t7])), images,t,p) = 
    return $ Pictures $ [Translate 0 (400-(t/5)) $ lfundo tf ,Translate 0 (300-(t/5)) $ lfundo t1,Translate 0 (200-(t/5))    $ lfundo t2, Translate 0 (100-(t/5))  $ lfundo t3,Translate 0 (0-(t/5)) $ lfundo t4, Translate 0 (-100-(t/5))  $ lfundo t5,Translate 0 (-200-(t/5))    $ lfundo t6, Translate 0 (-300-(t/5))  $ lfundo t7,Translate ((i*100)-300) (300 -(j*100)-(t/5)) $ boneco]                                                                                           --desenha o mapa de jogo
     where 
        i=fromIntegral x
        j=fromIntegral y
        boneco = if (t <25 )||(t>50 && t <75)||(t>100 && t<125 )||(t>150 && t<175 )||(t>200 &&t<225)  then (head images) else (last images)

drawOption option = Translate 0 0 $ Scale (0.5) (0.5) $ Text option 


river::Picture
river= color blue $ rectangleSolid 700 100

road::Picture
road= color  black $ rectangleSolid 700 100

grass::Picture
grass = color green $ rectangleSolid 700 100
     


lfundo::(Terreno,[Obstaculo])->Picture
lfundo (Rio v,obs)= river
lfundo (Estrada v,obs)= road
lfundo (Relva,obs)= grass





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
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Cima)),i,n,p)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Baixo)),i,n,p)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Esquerda)),i,n,p)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p)= 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Direita)),i,n,p)
event _ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n,p) = 
    return $ (ModoJogo,(animaJogo (Jogo (Jogador (x, y))(Mapa l to)) Parado) ,i,n,p) 
event _ w = return w

time :: Float -> World ->IO World
time f (m, j, i,249,p) = return $ (m, deslizaJogo p j,i, 0,p)
time f (m, j, i, t,p) = return $ (m, j, i,t+1,p)



main :: IO ()
main = do
 bonecoesq <- loadBMP "bonecoesq.bmp"
 bonecodir <- loadBMP "boneodir.bmp"
 let images = [scale 0.8 0.8 bonecoesq, scale 0.8 0.8 bonecodir]
 playIO window white  fr (initialState images) drawState event time








