{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{- |
Module      : Tarefa5_2021li1g066
Description : Aplicação Gráfica
Copyright   : Afonso Marques <a94940@alunos.uminho.pt>;
            : Luís Maia <a95656@alunos.uminho.pt>;

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2021/22.

== Notas da Tarefa5
     
     As principais dificuldades encontradas nesta tarefa devem-se ao facto de os exemplos dados pela equipa docente terem sido muito
     minimalistas, não abrangendo muitas das funcionalidades da biblioteca Gloss. Deste modo, foi necessária uma procura de informação
     mais profunda e foram usados métodos, algoritmos e ideias presentes nos vídeos de apoio disponibilizados pelo Cesium para a elaboração desta tarefa.
     Sendo uma Tarefa gráfica, a implementação de testes para a mesma não se justificou, tema abordado e esclarecido com a docente do nosso turno prático.

     == Fontes das imagens usadas:

     * imagem do jogador: <https://www.kindpng.com/imgv/TJmhwJh_2d-game-character-png-transparent-png/>
     * imagem da porta:   <https://pt.dreamstime.com/estilo-de-desenho-animado-medieval-vetorial-da-porta-do-castelo-isolado-em-branco-dobradi%C3%A7as-antigas-portas-e-pedras-s%C3%B3lidas-image161353249>
     * imagem do chão:    <https://www.mobly.com.br/papel-de-parede-tijolos-2d-natural-464727.html>
     * imagem da caixa:   <https://opengameart.org/content/top-down-2d-metal-box>
-}
module Main where

import LI12122
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Tarefa2_2021li1g066
import Tarefa4_2021li1g066
import Data.Maybe

{- | Foi criado o tipo de dados 'OpcoesMenu' com o objetivo de representar e desenhar as opções do menu principal do jogo.
-}
data OpcoesMenu 
  = Jogar -- ^ O construtor 'Jogar' é o que permite jogar o jogo. 
  | Sair  -- ^ O construtor 'Sair' é o que permite sair do menu.

{- | Adicionalmente criou-se o tipo de dados 'Nivel' para determinar o nível selecionado para jogar.
-}
data Nivel 
  = Um   -- ^ Construtor que se refere ao nível 1 
  | Dois -- ^ Construtor que se refere ao nível 2
  | Tres -- ^ Construtor que se refere ao nível 3

{- | Criou-se o tipo de dados 'Menu' no qual estão representados todos as possibilidades de estado do jogo de modo a
     ser possível desenhar todo o jogo.
-}
data Menu 
  = Controlar OpcoesMenu -- ^ O construtor 'Controlar' vai receber uma opção do menu para poder interagir com o mesmo.
  | EscolheNivel Nivel   -- ^ O construtor 'EscolheNivel' recebe um nível para o poder inicializar.
  | ModoJogo Nivel       -- ^ O construtor 'ModoJogo' recebe o nível escolhido para desenhar esse mesmo nível.
  | VenceuJogo           -- ^ O construtor 'VenceuJogo' serve para determinar quando o jogador concluiu o nível com sucesso.

{- | O tipo de dados 'Mundo' é um tuplo que armazena a informação do estado do menu na primeira componente, o jogo a ser inicializado
     na segunda componente e a terceira componente guarda as coordenadas da Porta do jogo de modo a futuramente se saber quando o jogo
     foi concluido com sucesso.
-}
type Mundo = (Menu, Jogo, Coordenadas) -- as coordenadas são as coordenadas da porta

{- | Por fim, o tipo de dados 'EstadoGloss' é o mais global, que armazena:

* O mundo;
* Uma lista de pares @(Peca,Picture)@ em que a primeira componente corresponde a um tipo de peça e a segunda à imagem correspondente;
* Uma lista de pares @(Direcao,Picture)@ em que a primeira componente corresponde à direção do jogador e a segunda à imagem correspondente.
-}
type EstadoGloss = (Mundo, [(Peca, Picture)], [(Direcao, Picture)])

{- | A função 'desenha' é uma das principais funções desta Tarefa, sendo ela responsável por desenhar todo o jogo, tendo em conta
     o tipo de Menu presente no estado a ser desenhado.
-}
desenha :: EstadoGloss -> Picture
desenha ((EscolheNivel Um, j, c),_,_) = Pictures [Color blue $ desenhaOpcoesMenu "Nivel 1", Translate (0) (-70) $ desenhaOpcoesMenu "Nivel 2", Translate (0) (-140) $ desenhaOpcoesMenu "Nivel 3"]
desenha ((EscolheNivel Dois, j, c),_,_) = Pictures [desenhaOpcoesMenu "Nivel 1", Color blue $ Translate (0) (-70) $ desenhaOpcoesMenu "Nivel 2", Translate (0) (-140) $ desenhaOpcoesMenu "Nivel 3"]
desenha ((EscolheNivel Tres, j, c),_,_) = Pictures [desenhaOpcoesMenu "Nivel 1", Translate (0) (-70) $ desenhaOpcoesMenu "Nivel 2", Color blue $ Translate (0) (-140) $ desenhaOpcoesMenu "Nivel 3"]
desenha ((VenceuJogo, j, c),_,_) = Translate (-200) 0 $ Color red $ Text "Ganhou!"
desenha ((Controlar Jogar, j, c),_,_) = Pictures [Color blue $ desenhaOpcoesMenu "Jogar", Translate (0) (-70) $ desenhaOpcoesMenu "Sair"]
desenha ((Controlar Sair, j, c),_,_) = Pictures [desenhaOpcoesMenu "Jogar", Color blue $ Translate (0) (-70) $ desenhaOpcoesMenu "Sair"]
desenha e@((ModoJogo n, j, c), pcs, jgs) = converteJogo e

{- | A função 'desenhaOpcoesMenu' é uma função auxiliar da função 'desenha' que recebe a string @s@ e converte-a numa Picture
     com o texto contido nessa string, de modo a apresentar o texto presente nos menus.
-}
desenhaOpcoesMenu :: String -> Picture
desenhaOpcoesMenu s = Translate (-50) 0 $ Scale (0.5) (0.5) $ Text s

{- | A função 'moverTecla' é uma das funções principais e é responsável pela atualização do estado do jogo tendo em conta a 
     tecla premida pelo jogador e o estado imediatamente anterior quando a tecla foi premida. 
-}
moverTecla :: Event -> EstadoGloss -> EstadoGloss
moverTecla (EventKey (SpecialKey KeyEnter) Down _ _) ((Controlar Jogar, jogo, c), pcs, jgs) = ((EscolheNivel Um, jogo, c), pcs, jgs)
moverTecla (EventKey (SpecialKey KeyUp) Down _ _) ((Controlar Jogar, jogo, c), pcs, jgs) = ((Controlar Sair, jogo, c), pcs, jgs)
moverTecla (EventKey (SpecialKey KeyDown) Down _ _) ((Controlar Jogar, jogo, c), pcs, jgs) = ((Controlar Sair, jogo, c), pcs, jgs)

moverTecla (EventKey (SpecialKey KeyEnter) Down _ _) ((EscolheNivel Um, jogo, c), pcs, jgs) = (estado1, pcs, jgs)
moverTecla (EventKey (SpecialKey KeyUp) Down _ _) ((EscolheNivel Um, jogo, c), pcs, jgs) = ((EscolheNivel Tres, jogo, c), pcs, jgs)
moverTecla (EventKey (SpecialKey KeyDown) Down _ _) ((EscolheNivel Um, jogo, c), pcs, jgs) = ((EscolheNivel Dois, jogo, c), pcs, jgs)

moverTecla (EventKey (SpecialKey KeyEnter) Down _ _) ((EscolheNivel Dois, jogo, c), pcs, jgs) = (estado2, pcs, jgs)
moverTecla (EventKey (SpecialKey KeyUp) Down _ _) ((EscolheNivel Dois, jogo, c), pcs, jgs) = ((EscolheNivel Um, jogo, c), pcs, jgs)
moverTecla (EventKey (SpecialKey KeyDown) Down _ _) ((EscolheNivel Dois, jogo, c), pcs, jgs) = ((EscolheNivel Tres, jogo, c), pcs, jgs)

moverTecla (EventKey (SpecialKey KeyEnter) Down _ _) ((EscolheNivel Tres, jogo, c), pcs, jgs) = (estado3, pcs, jgs)
moverTecla (EventKey (SpecialKey KeyUp) Down _ _) ((EscolheNivel Tres, jogo, c), pcs, jgs) = ((EscolheNivel Dois, jogo, c), pcs, jgs)
moverTecla (EventKey (SpecialKey KeyDown) Down _ _) ((EscolheNivel Tres, jogo, c), pcs, jgs) = ((EscolheNivel Um, jogo, c), pcs, jgs)

moverTecla (EventKey (SpecialKey KeyUp) Down _ _) ((Controlar Sair, jogo, c), pcs, jgs) = ((Controlar Jogar, jogo, c), pcs, jgs)
moverTecla (EventKey (SpecialKey KeyDown) Down _ _) ((Controlar Sair, jogo, c), pcs, jgs) = ((Controlar Jogar, jogo, c), pcs, jgs)
moverTecla (EventKey (SpecialKey KeyEnter) Down _ _) ((Controlar Sair, jogo, c), pcs, jgs) = undefined
moverTecla (EventKey (SpecialKey KeyEnter) Down _ _) ((VenceuJogo, jogo, c), pcs, jgs) = ((Controlar Jogar, jogo, c), pcs, jgs)
moverTecla e ((ModoJogo n, jogo@(Jogo m (Jogador cj d b)), c), pcs, jgs) = 
                  if cj == c then ((VenceuJogo, jogo, c), pcs, jgs)
                  else case e of (EventKey (SpecialKey KeyUp) Down _ _)   -> ((ModoJogo n, moveJogador jogo Trepar, c), pcs, jgs)
                                 (EventKey (SpecialKey KeyDown) Down _ _) -> ((ModoJogo n, moveJogador jogo InterageCaixa, c), pcs, jgs)
                                 (EventKey (SpecialKey KeyLeft) Down _ _) -> ((ModoJogo n, moveJogador jogo AndarEsquerda, c), pcs, jgs)
                                 (EventKey (SpecialKey KeyRight) Down _ _) -> ((ModoJogo n, moveJogador jogo AndarDireita, c), pcs, jgs)
                                 _ -> ((ModoJogo n, jogo, c), pcs, jgs)
moverTecla _ w = w                                    


{- | A função 'getMapa' é uma função auxiliar da função 'converteJogo' que recebe um estado e devolve o mapa contido nesse estado para
     depois converter as peças desse mapa numa imagem.
-}
getMapa :: Mundo -> Mapa
getMapa (_, (Jogo mapa jog), _ ) = mapa

{- | A constante 'lP' guarda o valor da largura das imagens que vão ser criadas para os mapas dos níveis 1 e 2.
-}
lP :: Float
lP = 100

{- | A constante 'lG' guarda o valor da largura das imagens que vão ser criadas para o mapa do nível 3.
-}
lG :: Float
lG = 50

{- | A constante 'alturaP' guarda o valor da ordenada onde as imagens vão começar a ser colocadas para os mapas dos níveis 1 e 2.
-}
alturaP :: Float
alturaP = 200

{- | A constante 'alturaG' guarda o valor da ordenada onde as imagens vão começar a ser colocadas para o mapa do nível 3.
-}
alturaG :: Float
alturaG = 300

{- | A constante 'comprimentoP' guarda o valor da abcissa onde as imagens vão começar a ser colocadas para os mapas dos níveis 1 e 2.
-}
comprimentoP :: Float
comprimentoP = (-300)

{- | A constante 'comprimentoG' guarda o valor da abcissa onde as imagens vão começar a ser colocadas para o mapa do nível 3.
-}
comprimentoG :: Float
comprimentoG = (-500)

{- | A constante 'escJogP' guarda o valor da escala para a imagem que representa o jogador para os mapas dos níveis 1 e 2.
-}
escJogP :: Float
escJogP = 0.22

{- | A constante 'escJogG' guarda o valor da escala para a imagem que representa o jogador para o mapa do nível 3.
-}
escJogG :: Float
escJogG = 0.09

{- | A constante 'escPecP' guarda o valor da escala para as imagens que vão substituir as peças dos mapas dos níveis 1 e 2.
-}
escPecP :: Float
escPecP = 0.45

{- | A constante 'escPecG' guarda o valor da escala para as imagens que vão substituir as peças do mapa do nível 3.
-}
escPecG :: Float
escPecG = 0.22


{- | A imagem 'vazioPic' é um simples quadrado branco que vai representar as peças Vazio do mapa
-}
vazioPic :: Picture 
vazioPic = Color white $ rectangleSolid lG lG

{- | A função 'desenhaJogador' é a função que recebe um Jogador e converte-o numa lista de imagens, colocando-as nas posições corretas,
     convertendo as coordenadas do Jogador. 
     Para tal recebe os valores de comprimento, altura, largura, escala das peças e escala dos jogadores para definir o tamanho com o qual a sua imagem vai ser 
     desenhada. Recebe também as listas que fazem o mapeamento entre as peças e as respetivas imagens e a direção do jogador com as
     respetivas imagens. O resultado é uma lista de Pictures pois caso o booleano do jogador que determina se o Jogador carrega caixa
     for True significa que se tem que adicionar uma imagem de uma caixa por exatamente por cima do Jogador.
-}
desenhaJogador :: Jogador -> Float -> Float -> Float -> Float -> [(Peca, Picture)] -> Float -> [(Direcao, Picture)] -> [Picture] 
desenhaJogador (Jogador (x,y) d b) cmp alt lrg ep pcs ej jgs
  | b = [Translate ((atualX x)-(lrg)) (atualY y) (scale ej ej ((fromJust . lookup d) jgs)), Translate ((atualX x)-(lrg)) ((atualY y) + (lrg)) (scale ep ep ((fromJust . lookup Caixa) pcs))]
  | otherwise = [Translate ((atualX x)-(lrg)) (atualY y) (scale ej ej ((fromJust . lookup d) jgs))]
  where atualX = (+ cmp) . (* (lrg)) . realToFrac . succ
        atualY = (+ alt) . (* (-(lrg))) . realToFrac

{- | A função 'desenhaPeca' é a função que recebe uma peça, a posição onde esta vai ser colocada, a lista onde está mapeada a imagem
     correspondente à peça e desenha a imagem com a escala correspondente.
-}
desenhaPeca :: Float -> Float -> Float -> Peca -> Float -> [(Peca,Picture)] -> Picture
desenhaPeca lrg x y peca ep pcs = Translate x y (scale ep ep ((fromJust . lookup peca ) pcs))

{- | A função 'desenhaLinha' é a função que vai desenhar uma linha do mapa, desenhando a imagem da primeira peça da linha através da função
     'desenhaPeca' e recursivamente desenha o resto da linha, atualizando a posição onde a imagem seguinte vai ser colocada.
-}
desenhaLinha :: Float -> Float -> Float -> [Peca] -> Float -> [(Peca, Picture)] -> [Picture]
desenhaLinha lrg x y (h:t) ep pcs = desenhaPeca lrg x y h ep pcs : desenhaLinha lrg (x+lrg) y t ep pcs
desenhaLinha _ _ _ _ _ _ = []

{- | A função 'desenhaMapa' é a função que vai converter um mapa na respetiva representação por imagens, convertendo a primeira linha 
     e recursivamente desenha o resto das linhas, atualizando a posição onde a linha seguinte vai ser colocada.
-}
desenhaMapa :: Float -> Float -> Float -> Mapa -> Float -> [(Peca, Picture)] -> [Picture]
desenhaMapa lrg x y (h:t) ep pcs = desenhaLinha lrg x y h ep pcs ++ desenhaMapa lrg x (y-lrg) t ep pcs
desenhaMapa _ _ _ _ _ _ = []

{- | A função 'converteJogo' é a função que vai converter o estado numa imagem, de modo a representar gráficamente os diferentes
     níveis do jogo, usando as constantes de tamanhos e escalas referentes ao nível selecionado.
-}
converteJogo :: EstadoGloss -> Picture
converteJogo (w@(modo, jogo, cp), pcs, jgs)  = Pictures desenhoDoJogo
   where desenhoDoJogo = desenhoDoMapa ++ desenhoDoJogador
         desenhoDoMapa = desenhaMapa lrg cm alt mapa ep pcs
         desenhoDoJogador = desenhaJogador j1 cm alt lrg ep pcs ej jgs
         j1 = getJogador w
         mapa = getMapa w
         cm = case modo of ModoJogo Tres -> comprimentoG
                           _ -> comprimentoP
         alt = case modo of ModoJogo Tres -> alturaG
                            _ -> alturaP
         lrg = case modo of ModoJogo Tres -> lG
                            _ -> lP
         ep = case modo of ModoJogo Tres -> escPecG
                           _ -> escPecP
         ej = case modo of ModoJogo Tres -> escJogG
                           _ -> escJogP

{- | A função 'getJogador' é uma função auxiliar da função 'converteJogo' que recebe um estado e devolve o jogador contido nesse estado para
     converter o jogador desse estado na respetiva imagem.
-}
getJogador :: Mundo -> Jogador
getJogador (_, (Jogo m j), _) = j

{- | Definição do gloss para que o jogo seja corrido em fullscreen.
-}
window :: Display 
window = FullScreen

{- | Definição do gloss para que define o frame rate do jogo.
-}
fr :: Int
fr = 25

{- | Definição do gloss que altera o estado do jogo consoante a passagem do tempo. Neste caso não ocorre nenhuma atualização.
-}
tempo :: Float -> EstadoGloss -> EstadoGloss
tempo _ w = w 

{- | Definição do gloss que define a cor do fundo da janela onde corre o jogo.
-}
background :: Color 
background = greyN 0.993

{- | A função 'constroiEstadoGloss' é uma função usada pela função 'main' que vai construir o estado inicial quando o jogo começa a ser executado.
-}
constroiEstadoGloss :: Mundo -> [(Peca, Picture)] -> [(Direcao, Picture)] -> EstadoGloss
constroiEstadoGloss m imgs jd = (m,imgs,jd)

{- | O 'estado1' é o estado que é carregado quando no menu é selecionado o nível 1.
-}
estado1 :: Mundo
estado1 = (ModoJogo Um, Jogo m1 j1, cp1)

{- | 'm1' é o mapa do nível 1.
-}
m1 :: Mapa
m1 =   [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
         [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Porta, Vazio, Vazio, Bloco, Vazio, Vazio, Bloco],
         [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco] ]

{- | 'j1' são as definições iniciais do jogador do nível 1.
-}
j1 :: Jogador
j1 = Jogador (6,0) Oeste False

{- | 'cp1' são as coordenadas da porta do mapa do nível 1.
-}
cp1 :: Coordenadas
cp1 = (0,3)

{- | O 'estado2' é o estado que é carregado quando no menu é selecionado o nível 2.
-}
estado2 :: Mundo
estado2 = (ModoJogo Dois, Jogo m2 j2, cp2)

{- | 'm2' é o mapa do nível 2.
-}
m2 :: Mapa
m2 =   [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
         [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Bloco],
         [Porta, Vazio, Bloco, Vazio, Caixa, Vazio, Bloco],
         [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco] ]

{- | 'j2' são as definições iniciais do jogador do nível 2.
-}
j2 :: Jogador
j2 = Jogador (6,0) Oeste False

{- | 'cp2' são as coordenadas da porta do mapa do nível 2.
-}
cp2 :: Coordenadas
cp2 = (0,3)

{- | O 'estado3' é o estado que é carregado quando no menu é selecionado o nível 3.
-}
estado3 :: Mundo
estado3 = (ModoJogo Tres, Jogo m3 j3, cp3)


{- | 'm3' é o mapa do nível 3.
-}
m3 :: Mapa
m3 = [   [Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio],
         [Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Caixa, Caixa, Caixa, Caixa, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Porta, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Bloco, Bloco, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Caixa, Bloco],
         [Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Vazio, Caixa, Caixa, Caixa, Bloco],
         [Vazio, Bloco, Vazio, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Vazio, Vazio, Caixa, Caixa, Caixa, Caixa, Bloco],
         [Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Vazio, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco],
         [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco, Bloco, Bloco, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
      ]

{- | 'j3' são as definições iniciais do jogador do nível 3.
-}
j3 :: Jogador
j3 = Jogador (12,8) Oeste False

{- | 'cp3' são as coordenadas da porta do mapa do nível 3.
-}
cp3 :: Coordenadas
cp3 = (1,8)

{- | O 'estadoInicial' é o estado usado para inicializar o gloss no modo Menu Controlar Jogar. O resto dos parâmetros são 
     meramente aleatórios e serão substituídos na escolha do nível.
-}
estadoInicial :: Mundo
estadoInicial = (Controlar Jogar, Jogo [] (Jogador (0,0) Este False), (1,1))

{- | Função principal do módulo, que faz o jogo ser executado.
-}
main :: IO ()
main = do
  bloco  <- loadBMP "img/chao.bmp"
  porta  <- loadBMP "img/porta.bmp"        
  caixa  <- loadBMP "img/caixa.bmp"
  jogdir <- loadBMP "img/jogdir1.bmp"
  jogesq <- loadBMP "img/jogesq1.bmp"
  play window 
       background 
       fr 
       ( constroiEstadoGloss 
          estadoInicial 
          [(Bloco, bloco),
           (Porta, porta),
           (Caixa, caixa),
           (Vazio, vazioPic)]
          [(Este, jogdir),
           (Oeste, jogesq)]
       ) 
       desenha 
       moverTecla 
       tempo

