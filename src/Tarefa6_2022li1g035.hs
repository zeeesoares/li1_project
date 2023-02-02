{- |
Module      : Tarefa5_2022li1g035
Description : Determinar se o jogo terminou
Copyright   : José António Costa Soares <a103995@alunos.uminho.pt>
              Nuno Miguel Parente Morais <a104090@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}

module Tarefa6_2022li1g035 where

import LI12223
import Tarefa1_2022li1g035
import Tarefa2_2022li1g035
import Tarefa3_2022li1g035 
import Tarefa4_2022li1g035
import Tarefa5_2022li1g035

import System.Random

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


-------------- Tipos utilizados no jogo ------------------

data Opcao = Jogar   -- Opções de menu
            | Skin
            | Sair
            | Menu

data Skins =  Galinha  -- Opções de skins
            | Macaco
            | Gato
            | Goat
            | Coelho


data Menu = Opcoes Opcao  --Opções de Jogo
          | ModoJogo 
          | ModoSkins Skins
          | PerdeuJogo Opcao
          | Pause


type Estado = (Jogo,Jogada)

type EstadoGloss = (Menu, Estado, Imagens, Picture, Int, Int)



data Imagens = 
  Imagens { galinha :: Picture
          , relva :: Picture
          , estrada :: Picture
          , rio :: Picture
          , arvore :: Picture
          , tronco :: Picture
          , carrodireita :: Picture
          , carroesquerda :: Picture
          , fundo :: Picture
          , logo :: Picture
          , start :: Picture
          , startPress :: Picture
          , exit :: Picture
          , exitPress :: Picture
          , skins :: Picture
          , skinsPress :: Picture
          , placaStart :: Picture
          , placaSkins :: Picture
          , placaExit :: Picture 
          , gato :: Picture
          , coelho :: Picture
          , macaco :: Picture
          , goat :: Picture
          , coin :: Picture
          , moldura :: Picture
          , menu :: Picture
          , menupress :: Picture
          , instrucoes :: Picture
          , ghost :: Picture
          }



---------------- IMAGENS -----------------

carregarImagens :: IO Imagens   -- Função responsável por carregar todas as imagens
carregarImagens = do 
  galinha <- loadBMP "images/galinha.bmp"
  gato <- loadBMP "images/gato.bmp"
  goat <- loadBMP "images/goat.bmp"
  macaco <- loadBMP "images/macaco.bmp"
  coelho <- loadBMP "images/coelho.bmp"
  relva <- loadBMP "images/relva.bmp"
  rio <- loadBMP "images/agua.bmp"
  estrada <- loadBMP "images/estrada.bmp"
  arvore <- loadBMP "images/arvore.bmp"
  tronco <- loadBMP "images/tronco.bmp"
  carrodireita <- loadBMP "images/carro.bmp"
  carroesquerda <- loadBMP "images/carroesquerda.bmp"
  logo <- loadBMP "images/logo.bmp"
  fundo <- loadBMP "images/fundo.bmp"
  start <- loadBMP "images/start1.bmp"
  startPress <- loadBMP "images/start2.bmp"
  exit <- loadBMP "images/exit.bmp"
  exitPress <- loadBMP "images/exit2.bmp"
  skins <- loadBMP "images/skins.bmp"
  skinsPress <- loadBMP "images/skins1.bmp"
  placaStart <- loadBMP "images/placaStart.bmp"
  placaSkins <- loadBMP "images/placaSkins.bmp"
  placaExit <- loadBMP "images/placaExit.bmp"
  coin <- loadBMP "images/coin.bmp"
  moldura <- loadBMP "images/moldura.bmp"
  menu <- loadBMP "images/menu.bmp"
  menupress <- loadBMP "images/menupress.bmp"
  instrucoes <- loadBMP "images/instrucoes.bmp"
  ghost <- loadBMP "images/ghost.bmp"
  return Imagens {galinha = galinha
                , relva = relva
                , rio = rio
                , estrada = estrada
                , arvore = arvore
                , tronco = tronco
                , carrodireita = carrodireita
                , carroesquerda = carroesquerda
                , fundo = Scale 9 9 fundo
                , logo = Scale 1 1 logo
                , start = start
                , startPress = startPress
                , exit = exit
                , exitPress = exitPress
                , skins = skins
                , skinsPress = skinsPress
                , placaStart = Scale 10 8  placaStart
                , placaSkins = Scale 10 8  placaSkins
                , placaExit = Scale 10 8 placaExit
                , goat = goat
                , gato = gato
                , macaco = macaco
                , coelho = coelho
                , coin = coin
                , moldura = moldura
                , menu = menu
                , menupress = menupress
                , instrucoes = instrucoes
                , ghost = ghost
                }

getMapa :: Mapa -> [(Terreno,[Obstaculo])]  -- Função que nos dá a lista de Terrenos para ser possivel trabalhar com a matriz de jogo
getMapa (Mapa _ lista) = lista



-------- Valores Especificos para a formação do mapa inicial --------

altura :: Float  -- posição da altura do primeiro bloco
altura = 500

comprimento :: Float  -- posição do comprimento do primeiro bloco
comprimento = -500

lado :: Float  -- lado de cada bloco
lado = 100




----------------------------- PARTE GRÀFICA ------------------------------------

desenhaMapaMenu :: EstadoGloss -> Picture  -- Função que  desenha o mapa do menu (Background)
desenhaMapaMenu t@(e, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = Pictures [Rotate 3 (Pictures (desenhaMapa (-950) 650 (getMapa mapaMenu) imagens)), Translate (675) (-470) (Scale 2.5 2.5 (instrucoes imagens))]


drawState :: EstadoGloss -> Picture   -- Função responsável por toda a parte gráfica do jogo

drawState t@(PerdeuJogo Jogar,(Jogo (Jogador (a,b)) mapa, jogada), imagens, personagem, score,scoremax) =
  Pictures (desenhaMapa (-500) 500 (getMapa mapa) imagens  ++ 
  [ desenhaScore t
  , Scale 12.5 12 (translate 0 0 (moldura imagens))
  , Translate (fromIntegral (a*100-500)) (fromIntegral (-1*(b*100 - 500))) (Scale 0.28 0.28 (ghost imagens))
  , Translate (-550) (-200) (placaStart imagens)
  , Translate (-550) 50 (startPress imagens)
  , Translate (-550) (-195) (menu imagens)
  , Translate (-550) (-445) (exit imagens)])

drawState t@(PerdeuJogo Menu,(Jogo (Jogador (a,b)) mapa, jogada), imagens, personagem, score,scoremax) =
    Pictures (desenhaMapa (-500) 500 (getMapa mapa) imagens  ++ 
    [desenhaScore t 
    , Scale 12.5 12 (translate 0 0 (moldura imagens))
    , Translate (fromIntegral (a*100-500)) (fromIntegral (-1*(b*100 - 500))) (Scale 0.28 0.28 (ghost imagens))
    , Translate (-550) (-200) (placaSkins imagens)
    , Translate (-550) 50 (start imagens)
    , Translate (-550) (-195) (menupress imagens)
    , Translate (-550) (-445) (exit imagens)])

drawState t@(PerdeuJogo Sair,(Jogo (Jogador (a,b)) mapa, jogada), imagens, personagem, score,scoremax) =
    Pictures (desenhaMapa (-500) 500 (getMapa mapa) imagens  ++ 
    [ desenhaScore t 
    , Scale 12.5 12 (translate 0 0 (moldura imagens))
    , Translate (fromIntegral (a*100-500)) (fromIntegral (-1*(b*100 - 500))) (Scale 0.28 0.28 (ghost imagens))
    , Translate (-550) (-200) (placaExit imagens)
    , Translate (-550) 50 (start imagens)
    , Translate (-550) (-195) (menu imagens)
    , Translate (-550) (-445) (exitPress imagens)])
  

drawState t@(Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures (desenhaMapaMenu t :[Translate 0 410 (Scale 1.2 1.2 (logo imagens))   -- Desenha Evento Opções de Menu
  , Translate 0 (-200) (placaStart imagens)
  , Translate 0 50 (startPress imagens)
  , Translate 0 (-195) (skins imagens)
  , Translate 0 (-445) (exit imagens)
  ])

drawState t@(Opcoes Skin, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score,scoremax) =
   Pictures (desenhaMapaMenu t :[Translate 0 410 (Scale 1.2 1.2 (logo imagens))   -- Desenha Evento Opções de Menu
  , Translate 0 (-200) (placaSkins imagens)
  , Translate 0 50 (start imagens)
  , Translate 0 (-195) (skinsPress imagens)
  , Translate 0 (-445) (exit imagens)])

drawState t@(Opcoes Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures (desenhaMapaMenu t :[Translate 0 410 (Scale 1.2 1.2 (logo imagens))  -- Desenha Evento Opções de Menu
  , Translate 0 (-200) (placaExit imagens) 
  , Translate 0 50 (start imagens)
  , Translate 0 (-195) (skins imagens)
  , Translate 0 (-445) (exitPress imagens)])
                                                                                
drawState t@(ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures [Translate (-450) 0 (Scale 12 12 (relva imagens))  -- Desenha Evento Opções de Skins
  ,Translate 450 0 (Scale 12 12 (relva imagens))
  ,Translate (-500) 200 (Scale 3 3 (coin imagens))
  , drawMenuSkins t] 

drawState t@(ModoSkins Gato, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures [Translate (-450) 0 (Scale 12 12 (relva imagens))  -- Desenha Evento Opções de Skins
  ,Translate 450 0 (Scale 12 12 (relva imagens))
  ,Translate 0 200 (Scale 3 3 (coin imagens))
  , drawMenuSkins t]

drawState t@(ModoSkins Goat, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures [Translate (-450) 0 (Scale 12 12 (relva imagens))  -- Desenha Evento Opções de Skins
  ,Translate 450 0 (Scale 12 12 (relva imagens))
  ,Translate 500 200 (Scale 3 3 (coin imagens))
  , drawMenuSkins t]

drawState t@(ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures [Translate (-450) 0 (Scale 12 12 (relva imagens))   -- Desenha Evento Opções de Skins
  ,Translate 450 0 (Scale 12 12 (relva imagens))
  ,Translate (-250) (-250) (Scale 3 3 (coin imagens))
  , drawMenuSkins t]

drawState t@(ModoSkins Coelho, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures [Translate (-450) 0 (Scale 12 12 (relva imagens))  -- Desenha Evento Opções de Skins
  ,Translate 450 0 (Scale 12 12 (relva imagens))
  ,Translate 250 (-250) (Scale 3 3 (coin imagens))
  , drawMenuSkins t]

drawState t@(Pause, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures (desenhoMapa ++ [desenhaJogador t]++ [desenhaScore t, Scale 12 12 (translate 0 0 (moldura imagens))])  -- Desenha Evento Opções de Pausa
    where
    i = fromIntegral x
    j = fromIntegral y
    desenhoMapa =  desenhaMapa comprimento altura mapaInicial imagens
    mapaInicial =  getMapa mapa



    -- Desenha Evento Modo de Jogo
drawState t@(ModoJogo,(Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  Pictures (desenhoMapa ++ [desenhaJogador t] ++ [(Translate (1000) 0 pecapreta),desenhaScore t, Scale 12.5 12 (translate 0 0 (moldura imagens))] ++ [Translate (-1000) 0 pecapreta])
  where
    i = fromIntegral x
    j = fromIntegral y
    desenhoMapa =  desenhaMapa comprimento altura mapaInicial imagens
    mapaInicial =  getMapa mapa


desenhaScore :: EstadoGloss -> Picture  -- Função que desenha o score do jogo
desenhaScore (_, _, imagens, _, score,scoremax) = Pictures [Translate 700 400 $ Color white $ scale 0.2 0.2 $ Text  ("Score : " ++ show score),
  Translate 650 350 $ Color white $ scale 0.2 0.2 $ Text  ("Max. Score: " ++ show scoremax)]



desenhaJogador :: EstadoGloss -> Picture  -- Função que desenha o jogador no mapa
desenhaJogador (_,a@(Jogo (Jogador (x,y)) _, _), imagens,personagem, score, scoremax) = translate (fromIntegral (x*100-500)) (fromIntegral (-1*(y*100 - 500))) (Scale 0.5 0.5 personagem)
 
drawMenuSkins :: EstadoGloss -> Picture   -- Função que desenha o menu de skins
drawMenuSkins (_, _, imagens,_,_,_)= Pictures [Translate (-500) 200 (Scale 1.5 1.5 (galinha imagens))
                       ,Translate 0 200 (Scale 1.5 1.5 (gato imagens))
                       ,Translate 500 200 (Scale 1.5 1.5 (goat imagens))
                       ,Translate (-250) (-250) (Scale 1.5 1.5 (macaco imagens))
                       ,Translate 250 (-250) (Scale 1.5 1.5 (coelho imagens))]


desenhaMapa :: Float -> Float -> [(Terreno,[Obstaculo])] -> Imagens -> [Picture] -- Função principal da desenha mapa
desenhaMapa x y (h:t) imagens = linha ++ resto
  where linha = desenhaLinha x y h imagens
        resto = desenhaMapa x (y-lado) t imagens
desenhaMapa _ _ _ _ = []

desenhaLinha :: Float -> Float -> (Terreno,[Obstaculo]) -> Imagens -> [Picture] -- Função que desenha um terreno
desenhaLinha x y (t,(ob:obs)) imagens = peca: resto
  where peca  = desenhaPeca x y (t,ob) imagens
        resto = desenhaLinha (x+lado) y (t, obs) imagens
desenhaLinha _ _ _ _ = []

desenhaPeca :: Float -> Float -> (Terreno,Obstaculo) -> Imagens -> Picture  -- -- Função que desenha um bloco
desenhaPeca x y (Relva,obs) imagens 
  | obs == Nenhum = translate x y (relva imagens)
  | otherwise = translate x y (arvore imagens)
desenhaPeca x y (Rio _,obs) imagens 
  | obs == Nenhum = translate x y (rio imagens)
  | otherwise = translate x y (tronco imagens)
desenhaPeca x y (Estrada v,obs) imagens 
  | obs == Nenhum = translate x y (estrada imagens)
  | obs == Carro && v >= 0 = translate x y (carrodireita imagens)
  | otherwise = translate x y (carroesquerda imagens)

pecapreta :: Picture
pecapreta = rectangleSolid 700 1200

-------- Mapas Utilizados ----------

mapa :: Mapa  -- Mapa inicial
mapa = Mapa 11 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Rio (0),[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum]),
               (Estrada (-2),[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Estrada 0,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum]),
               (Estrada 0,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
               ]

mapaMenu :: Mapa  -- Mapa do menu (fundo)
mapaMenu = Mapa 12 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ,(Estrada 3,[Carro,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum])
                   ,(Estrada (-3),[Carro,Nenhum,Carro,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Carro,Carro])
                   ,(Relva,[Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ,(Relva,[Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ,(Relva,[Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ,(Rio (-5),[Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Nenhum,Tronco,Tronco])
                   ,(Rio 2,[Nenhum,Nenhum,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Nenhum,Nenhum,Tronco,Nenhum,Tronco,Tronco,Nenhum])
                   ,(Estrada 3,[Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Carro,Nenhum,Nenhum,Nenhum,Carro,Nenhum])
                   ,(Estrada (-3),[Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Carro,Carro,Nenhum,Nenhum,Nenhum,Carro])
                   ,(Relva,[Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Arvore,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ,(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ,(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ,(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Arvore,Arvore,Nenhum,Arvore,Nenhum,Arvore,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])
                   ]


----- Funções da ReageEventoGloss ---------


estadoInicial :: Estado  -- Estado Inicial fixo
estadoInicial = (Jogo (Jogador (5,5)) mapa, Parado)

estadoGlossInicial :: Imagens -> EstadoGloss -- Estado Inicial (inicio de jogo)
estadoGlossInicial imagens = (Opcoes Jogar,estadoInicial, imagens, galinha imagens, 0, 0)

estadoGlossInicialPerdeu :: Imagens -> Picture -> Int -> EstadoGloss -- Estado Inicial depois de perder
estadoGlossInicialPerdeu imagens personagem scoremax = (ModoJogo ,estadoInicial, imagens, personagem, 0, scoremax)

estadoGlossInicialSaiu :: Imagens -> Picture -> Int -> EstadoGloss -- Estado Inicial quando se sai para o menu
estadoGlossInicialSaiu imagens personagem scoremax = (Opcoes Jogar ,estadoInicial, imagens, personagem, 0, scoremax)



reageEvento :: Event -> EstadoGloss -> EstadoGloss 
-- reageEvento menu (opçao jogar)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) =  
  (ModoJogo , (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)=  
  (Opcoes Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
   (Opcoes Skin, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento menu (opçao skins)   
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Skin, (Jogo (Jogador (x,y)) mapa, jogada), imagens,personagem, score, scoremax) =
    (ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Skin, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)=
    (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Skin, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
   (Opcoes Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento menu (opçao sair)
reageEvento (EventKey key Down _ _) (ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = case key of
        SpecialKey KeyUp ->  (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyDown ->  (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyRight ->  (ModoSkins Gato, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyLeft ->  (ModoSkins Goat, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyEsc ->  estadoGlossInicialSaiu imagens personagem scoremax
        SpecialKey KeyEnter ->  (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, galinha imagens, score, scoremax)
        _ ->  (ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento menu skins       
reageEvento (EventKey key Down _ _) (ModoSkins Gato, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = case key of
        SpecialKey KeyUp ->  (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyDown ->  (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyRight ->  (ModoSkins Goat, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyLeft ->  (ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyEsc ->  estadoGlossInicialSaiu imagens personagem scoremax
        SpecialKey KeyEnter ->  (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, gato imagens, score, scoremax)
        _ ->  (ModoSkins Gato, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento menu skins   
reageEvento (EventKey key Down _ _) (ModoSkins Goat, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = case key of
        SpecialKey KeyUp ->  (ModoSkins Coelho, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyDown ->  (ModoSkins Coelho, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyRight ->  (ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyLeft ->  (ModoSkins Gato, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyEsc ->  estadoGlossInicialSaiu imagens personagem scoremax
        SpecialKey KeyEnter ->  (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, goat imagens, score, scoremax)
        _ ->  (ModoSkins Goat, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento menu skins   
reageEvento (EventKey key Down _ _) (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = case key of
        SpecialKey KeyUp ->  (ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyDown ->  (ModoSkins Galinha, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyRight ->  (ModoSkins Coelho, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyLeft ->  (ModoSkins Coelho, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyEsc ->  estadoGlossInicialSaiu imagens personagem scoremax
        SpecialKey KeyEnter ->  (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, macaco imagens, score, scoremax)
        _ ->  (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento menu skins   
reageEvento (EventKey key Down _ _) (ModoSkins Coelho, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = case key of
        SpecialKey KeyUp ->  (ModoSkins Goat, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyDown ->  (ModoSkins Goat, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyRight ->  (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyLeft ->  (ModoSkins Macaco, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
        SpecialKey KeyEsc ->  estadoGlossInicialSaiu imagens personagem scoremax     
        SpecialKey KeyEnter ->  (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, coelho imagens, score, scoremax)
        _ ->  (ModoSkins Coelho, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento menu (opçao sair)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (Opcoes Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (Opcoes Skin, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (Opcoes Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (Opcoes Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (Opcoes Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  error "Fim de Jogo"

-- reageEvento menu perdeu (opçao jogar)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (PerdeuJogo Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (PerdeuJogo Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (PerdeuJogo Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (PerdeuJogo Menu, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  if score >= scoremax then estadoGlossInicialPerdeu imagens personagem score else estadoGlossInicialPerdeu imagens personagem scoremax

-- reageEvento menu perdeu (opçao menu)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (PerdeuJogo Menu, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (PerdeuJogo Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (PerdeuJogo Menu, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (PerdeuJogo Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo Menu, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  if score >= scoremax then estadoGlossInicialSaiu imagens personagem score else estadoGlossInicialSaiu imagens personagem scoremax

-- reageEvento menu perdeu (opçao sair)
reageEvento (EventKey (SpecialKey KeyUp) Down _ _) (PerdeuJogo Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (PerdeuJogo Menu, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyDown) Down _ _) (PerdeuJogo Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  (PerdeuJogo Jogar, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)
reageEvento (EventKey (SpecialKey KeyEnter) Down _ _) (PerdeuJogo Sair, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
  error "Fim de Jogo"

-- reageEvento para pausar o jogo
reageEvento (EventKey (SpecialKey KeySpace) Down _ _) (Pause, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) = 
   (ModoJogo, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

-- reageEvento para o modo de jogo
reageEvento (EventKey k Down _ _) t@(ModoJogo, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) 
  | k == SpecialKey KeyEsc =  ( (estadoGlossInicialSaiu imagens personagem scoremax))
  | k == SpecialKey KeySpace =  (Pause, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) 
  | otherwise =  (newState k t)

reageEvento _ (menu, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax) =  (menu, (Jogo (Jogador (x,y)) mapa, jogada), imagens, personagem, score, scoremax)

newState :: Key -> EstadoGloss -> EstadoGloss
newState k (menu , (Jogo (Jogador (x,y)) mapa, jogada), a, b, score, scoremax) =
  if not (jogoTerminou (Jogo (moveJogador (Jogador (x,y)) jogada mapa) mapa)) 
  then  
                         case k of
                         (SpecialKey KeyUp) -> if jogadaValida (Jogador (x,y)) (Move Cima) mapa == moveJogador (Jogador (x,y)) (Move Cima) mapa
                          then if not (jogoTerminou (Jogo (jogadaValida (Jogador (x,y)) (Move Cima) mapa) mapa)) 
                            then (menu , (Jogo (jogadaValida (Jogador (x,y)) (Move Cima) mapa) mapa, jogada), a, b, score + 1, scoremax) 
                            else (PerdeuJogo Jogar , (Jogo (jogadaValida (Jogador (x,y)) (Move Cima) mapa) mapa, jogada), a, b, score + 1, scoremax)
                          else (menu , (Jogo (jogadaValida (Jogador (x,y)) (Move Cima) mapa) mapa, jogada), a, b, score, scoremax)
                         (SpecialKey KeyDown) -> if jogadaValida (Jogador (x,y)) (Move Baixo) mapa == moveJogador (Jogador (x,y)) (Move Baixo) mapa
                          then if not (jogoTerminou (Jogo (jogadaValida (Jogador (x,y)) (Move Baixo) mapa) mapa)) 
                            then (menu , (Jogo (jogadaValida (Jogador (x,y)) (Move Baixo) mapa) mapa, jogada), a, b, score - 1, scoremax) 
                            else (PerdeuJogo Jogar , (Jogo (jogadaValida (Jogador (x,y)) (Move Baixo) mapa) mapa, jogada), a, b, score - 1, scoremax)
                          else (menu , (Jogo (jogadaValida (Jogador (x,y)) (Move Baixo) mapa) mapa, jogada), a, b, score, scoremax)
                         (SpecialKey KeyLeft) -> if not (jogoTerminou (Jogo (jogadaValida (Jogador (x,y)) (Move Esquerda) mapa) mapa)) 
                          then (menu , (Jogo (jogadaValida (Jogador (x,y)) (Move Esquerda) mapa) mapa, jogada), a, b, score, scoremax) 
                          else (PerdeuJogo Jogar , (Jogo (jogadaValida (Jogador (x,y)) (Move Esquerda) mapa) mapa, jogada), a, b, score, scoremax)
                         (SpecialKey KeyRight) -> if not (jogoTerminou (Jogo (jogadaValida (Jogador (x,y)) (Move Direita) mapa) mapa)) 
                          then (menu , (Jogo (jogadaValida (Jogador (x,y)) (Move Direita) mapa) mapa, jogada), a, b, score, scoremax) 
                          else (PerdeuJogo Jogar , (Jogo (jogadaValida (Jogador (x,y)) (Move Direita) mapa) mapa, jogada), a, b, score, scoremax)
                         _ -> (menu , (Jogo (jogadaValida (Jogador (x,y)) jogada mapa) mapa, jogada), a, b, score, scoremax)
  else (PerdeuJogo Jogar , (Jogo (jogadaValida (Jogador (x,y)) jogada mapa) mapa, jogada), a, b, score, scoremax)
  