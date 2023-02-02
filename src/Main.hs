module Main where

import LI12223
import Tarefa1_2022li1g035
import Tarefa2_2022li1g035
import Tarefa3_2022li1g035 
import Tarefa4_2022li1g035
import Tarefa5_2022li1g035
import Tarefa6_2022li1g035

import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LI12223

desenhaEstado :: EstadoGloss -> IO Picture
desenhaEstado t = return (drawState t)

reageEventoGloss :: Event -> EstadoGloss -> IO EstadoGloss
reageEventoGloss x y = return (reageEvento x y)

reageTempo :: Float -> EstadoGloss -> IO EstadoGloss
reageTempo _ s@(ModoJogo , (j@(Jogo (Jogador (x,y)) mapa), jogada), imagens, personagem, score,scoremax) = do
  seed <- randomRIO (1,50)
  if jogoTerminou j || length (getMapa mapa) == y+1
            then return (PerdeuJogo Jogar,(Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score,scoremax)
            else return (ModoJogo, (deslizaJogo (animaJogo j jogada) seed, jogada), imagens, personagem, score,scoremax) 
reageTempo _ (menu,(jogo,jogada), imagens, personagem, score,scoremax) = return (menu,(jogo,jogada), imagens, personagem,score,scoremax)

fr :: Int
fr = 1

dm :: Display
dm = FullScreen

main :: IO ()
main = do
  imagens <- carregarImagens
  playIO
    dm -- janela onde irá decorrer o jogo
    black -- cor do fundo da janela
    fr -- frame rate
    (estadoGlossInicial imagens) -- define estado inicial do jogo
    desenhaEstado-- desenha o estado do jogo
    reageEventoGloss -- reage a um evento
    reageTempo -- reage ao passar do tempo
