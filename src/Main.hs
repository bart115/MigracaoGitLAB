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

type World = (Menu, Jogo,Images, Time)

type Time = Float

type Images = [Picture]

window :: Display
window = InWindow "CrossyRoad" (440, 800) (0,0)

fr :: Int
fr = 50

initialState :: Images ->World
initialState images = (Opcoes Normal,( Jogo (Jogador (5,19)) (Mapa 11 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]), 
    (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])),images, 0)

--player::Picture
--player= rectangleSolid 20 20

drawState :: World ->IO Picture
drawState (PerdeuJogo, jogo, images, n) =return $  Scale (0.5) (0.5) $ Translate (-350) 0 $ Color red $ Text "Take the L"                                                                                                --desenha o estado perdeujogo
drawState (Pause ,jogo,images,n)= return $ Pictures [color red $ Translate (-10) 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)],color red $ Translate 20 0 $ polygon [(0,(-40)),((-16),(-40)),((-16),0),(0,0)]]   --desenha o estado pause
drawState (Opcoes Normal, jogo, images,n) = return $ Pictures [Color blue $ Translate (-110) 0 $ drawOption "Normal", Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]           --desenha o menu das opçoes para jogar normal
drawState (Opcoes Natal, jogo, images,n) =return $  Pictures [Translate (-110) 0 $ drawOption "Normal",Color blue $ Translate (-110) (-70) $ drawOption "Natal",Translate (-110) (-140) $ drawOption "Sair"]             --desenha o menu das opçoes para jogar natal
drawState (Opcoes Sair, jogo, images,n) = return $ Pictures [Translate (-110) 0 $ drawOption "Normal",Translate (-110) (-70) $ drawOption "Natal", Color blue $ Translate (-110) (-140) $ drawOption "Sair"]             --desenha o menu das opçoes para sair
drawState (ModoJogo,(Jogo (Jogador (x,y)) (Mapa l to)), images,n) = return $ Pictures $ [Translate ((i*40)-200) (380 -(j*40))  $ boneco]                                                                                               --desenha o mapa de jogo
     where 
        i=fromIntegral x
        j=fromIntegral y
        boneco = if (mod (round (n*1000)) 200) < 100 then (head images) else (last images)
drawOption option = Translate 0 0 $ Scale (0.5) (0.5) $ Text option 


event :: Event -> World -> IO World
-- Menu
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Normal, jogo,i,n) = return $ (ModoJogo, jogo,i,n)                       --passa do menu das opçoes para o jogo
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Natal, jogo,i,n) = return $ (ModoJogo, jogo,i,n)                        --passa do menu das opçoes para o jogo 
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Normal, jogo,i,n) = return $ (Opcoes Sair, jogo,i,n)                       --passa da opção jogar normal para a opção sair
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, jogo,i,n) = return $ (Opcoes Natal, jogo,i,n)                        --passa da opção sair para a opção jogar natal
event (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Natal, jogo,i,n) = return $ (Opcoes Normal, jogo,i,n)                      --passa da opção jogar natal para a opção jogar normal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Normal, jogo,i,n) = return $ (Opcoes Natal, jogo,i,n)                    --passa da opção jogar normal para a opção jogar natal
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Natal, jogo,i,n) = return $ (Opcoes Sair, jogo,i,n)                      --passa da opção jogar natal para a opção sair
event (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, jogo,i,n) = return $ (Opcoes Normal, jogo,i,n)                     --passa da opção sair para a opção jogar normal
event (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, jogo,i,n) =                                                       --sai do jogo
    do exitSuccess
event (EventKey (SpecialKey KeySpace) Down _ _) (ModoJogo,jogo,i,n) = return $ (Pause , jogo ,i,n)
event (EventKey (SpecialKey KeySpace) Down _ _) (Pause ,jogo,i,n) =return $  (ModoJogo , jogo ,i,n) 
event (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo, jogo,i,n) =return $  (Opcoes Normal,(Jogo (Jogador (5, 19)) (Mapa 11 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])) ,i,n)
event (EventKey (SpecialKey KeySpace) Down _ _) (PerdeuJogo, jogo,i,n) = return $ (Opcoes Normal,(Jogo (Jogador (5, 19)) (Mapa 11 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])])) ,i,n)
event (EventKey (SpecialKey KeyUp) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n)   = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Cima)),i,n)
event (EventKey (SpecialKey KeyDown) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Baixo)),i,n)
event (EventKey (SpecialKey KeyLeft) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n) = 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Esquerda)),i,n)
event (EventKey (SpecialKey KeyRight) Down _ _) (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n)= 
    if jogoTerminou (Jogo (Jogador (x, y)) (Mapa l to))==True then return $ (PerdeuJogo,(Jogo (Jogador (x, y)) (Mapa l to)),i,n) else return $ (ModoJogo, (animaJogo (Jogo (Jogador (x, y))(Mapa l to)) (Move Direita)),i,n)
event _ (ModoJogo, (Jogo (Jogador (x, y)) (Mapa l to)),i,n) = 
    return $ (ModoJogo,(animaJogo (Jogo (Jogador (x, y))(Mapa l to)) Parado) ,i,n) 
event _ w = return w

time :: Float -> World ->IO World
time t (o, j, i, n) = return $ (o, j, i, n+t)

main :: IO ()
main = do
 bonecoesq <- loadBMP "bonecoesq.bmp"
 bonecodir <- loadBMP "boneodir.bmp"
 let images = [scale 0.8 0.8 bonecoesq, scale 0.8 0.8 bonecodir]
 playIO window green  fr (initialState images) drawState event time








