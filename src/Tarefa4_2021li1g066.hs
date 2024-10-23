{- |
Module      : Tarefa4_2021li1g066
Description : Movimentação do personagem
Copyright   : Afonso Marques <a94940@alunos.uminho.pt>;
            : Luís Maia <a95656@alunos.uminho.pt>;

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2021/22.
-}
module Tarefa4_2021li1g066 where

import LI12122 
import Tarefa3_2021li1g066
import Tarefa2_2021li1g066

{- | A função 'getPecaMapa' é uma função auxiliar muito útil nesta tarefa, que recebe umas coordenadas e seleciona a peça
do mapa @m@ correspondente.
-}
getPecaMapa :: Coordenadas -> Mapa -> Peca
getPecaMapa (x,y) m = (m!!y)!!x 

{- | A funçãp 'temObstaculo' é uma função muito importante nesta tarefa. Consiste em averiguar se existe um obstáculo no mapa
do jogo @j@ para que o movimento @mov@ possa ou não acontecer. Se o resultado for @True@ significa que existe um obstáculo ao
movimento, não podendo este ser efetuado.
-}
temObstaculo :: Jogo -> Movimento -> Bool
temObstaculo j mov = 
       case mov of AndarDireita  -> obsAndarDir j 
                   AndarEsquerda -> obsAndarEsq j 
                   Trepar        -> obsTrepar j
                   InterageCaixa -> obsInteragirCaixa j

{- | A função 'obsAndarDir' é a função que diz se existe um obstáculo que impeça o movimento do jogador para a direita,
estando ou não estando este a carregar uma caixa. Uma condição importante é quando o jogador já se encontra na última coluna
do mapa, não podendo avançar para a direita, expressa na expressão @length (head mapa)@ .
-}
obsAndarDir :: Jogo -> Bool
obsAndarDir (Jogo mapa (Jogador (x,y) dir tc)) =
   case tc of False -> (x+1) == length (head mapa) || (getPecaMapa (x+1,y) mapa) == Bloco || (getPecaMapa (x+1,y) mapa) == Caixa 
              True  -> (x+1) == length (head mapa) || (getPecaMapa (x+1,y) mapa) == Bloco || (getPecaMapa (x+1,y) mapa) == Caixa || (getPecaMapa (x+1,y-1) mapa) == Caixa || (getPecaMapa (x+1,y) mapa) == Bloco 

{- | A função 'obsAndarEsq' é a função que diz se existe um obstáculo que impeça o movimento do jogador para a esquerda,
estando ou não estando este a carregar uma caixa. Uma condição importante é quando o jogador já se encontra na primeira coluna
do mapa, não podendo avançar para a esquerda, expressa na expressão @x == 0@ .
-}
obsAndarEsq :: Jogo -> Bool
obsAndarEsq (Jogo mapa (Jogador (x,y) dir tc)) =
   case tc of False -> x == 0 || (getPecaMapa (x-1,y) mapa) == Bloco || (getPecaMapa (x-1,y) mapa) == Caixa 
              True  -> x == 0 || (getPecaMapa (x-1,y) mapa) == Bloco || (getPecaMapa (x-1,y) mapa) == Caixa || (getPecaMapa (x-1,y-1) mapa) == Caixa || (getPecaMapa (x-1,y-1) mapa) == Bloco 

{- | A função 'obsInteragirCaixa' é a função que diz se existe um obstáculo que impeça a interação do jogador com uma caixa, estando
ou não estando este a carregar uma caixa. Uma condição importante é que o jogador tem que estar virado para a caixa para poder 
pegar nela. Caso o jogador já transporte uma caixa, tem que ver se há algum obstáculo para a deixar cair.
-}
obsInteragirCaixa :: Jogo -> Bool
obsInteragirCaixa (Jogo mapa (Jogador (x,y) dir tc)) = 
    case tc of False -> case dir of Este  -> (x+1) == length (head mapa) || ((getPecaMapa (x+1,y) mapa) /= Caixa || (getPecaMapa (x,y-1) mapa) /= Vazio || (getPecaMapa (x+1,y-1) mapa) /= Vazio) 
                                    Oeste -> x == 0 || (getPecaMapa (x-1,y) mapa) /= Caixa || (getPecaMapa (x,y-1) mapa) /= Vazio || (getPecaMapa (x-1,y-1) mapa) /= Vazio 
               True  -> case dir of Este  -> (x+1) == length (head mapa) || (getPecaMapa (x+1,y-1) mapa) /= Vazio 
                                    Oeste -> x == 0 || (getPecaMapa (x-1,y-1) mapa) /= Vazio 

{- | A função 'obsTrepar' é a função que diz se existe um obstáculo que impeça a ação de trepar caixas e blocos, estando
ou não estando este a carregar uma caixa. Caso o jogador esteja a transportar uma caixa, tem que ver há espaço para si e 
para a caixa de modo a poder subir.
-}
obsTrepar :: Jogo -> Bool
obsTrepar (Jogo mapa (Jogador (x,y) dir tc)) =
   case tc of False -> case dir of Este  -> ((getPecaMapa (x+1,y) mapa) /= Caixa && (getPecaMapa (x+1,y) mapa) /= Bloco) || (getPecaMapa (x+1,y-1) mapa) == Caixa || (getPecaMapa (x+1,y-1) mapa) == Bloco 
                                   Oeste -> ((getPecaMapa (x-1,y) mapa) /= Caixa && (getPecaMapa (x-1,y) mapa) /= Bloco) || (getPecaMapa (x-1,y-1) mapa) == Caixa || (getPecaMapa (x-1,y-1) mapa) == Bloco
              True  -> case dir of Este  -> ((getPecaMapa (x+1,y) mapa) /= Caixa && (getPecaMapa (x+1,y) mapa) /= Bloco) || (getPecaMapa (x+1,y-1) mapa) == Caixa || (getPecaMapa (x+1,y-1) mapa) == Bloco || (getPecaMapa (x+1,y-2) mapa) == Caixa || (getPecaMapa (x+1,y-2) mapa) == Bloco
                                   Oeste -> ((getPecaMapa (x-1,y) mapa) /= Caixa && (getPecaMapa (x-1,y) mapa) /= Bloco) || (getPecaMapa (x-1,y-1) mapa) == Caixa || (getPecaMapa (x-1,y-1) mapa) == Bloco || (getPecaMapa (x-1,y-2) mapa) == Caixa || (getPecaMapa (x-1,y-2) mapa) == Bloco

{- | A função 'moveEsq' é a função que faz o jogador se mover para a esquerda caso não haja nenhum obstáculo que impeça este movimento.
-}
moveEsq :: Jogo -> Jogador
moveEsq (Jogo mapa (Jogador (x,y) dir tc)) =
        case dir of Este  -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) AndarEsquerda)) then (Jogador (x-1, y) Oeste tc) else Jogador (x, y) Oeste tc
                    Oeste -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) AndarEsquerda)) then (Jogador (x-1, y) Oeste tc) else Jogador (x, y) Oeste tc

{- | A função 'moveDir' é a função que faz o jogador se mover para a direita caso não haja nenhum obstáculo que impeça este movimento.
-}
moveDir :: Jogo -> Jogador  
moveDir (Jogo mapa (Jogador (x,y) dir tc)) = 
    case dir of Este  -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) AndarDireita)) then Jogador (x+1, y) Este tc else Jogador (x, y) Este tc
                Oeste -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) AndarDireita)) then Jogador (x+1, y) Este tc else Jogador (x, y) Este tc

{- | A função 'trepa' é a função que faz o jogador trepar caixas e blocos, caso não haja nenhum obstáculo que impeça este movimento.
-}
trepa :: Jogo -> Jogador
trepa (Jogo mapa (Jogador (x,y) dir tc)) = 
    case tc of False -> case dir of Este  -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) Trepar)) then Jogador (x+1,y-1) Este tc else Jogador (x,y) Este tc
                                    Oeste -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) Trepar)) then Jogador (x-1,y-1) Oeste tc else Jogador (x,y) Oeste tc
               True  -> case dir of Este  -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) Trepar)) then Jogador (x+1,y-1) Este tc else Jogador (x,y) Este tc
                                    Oeste -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) Trepar)) then Jogador (x-1,y-1) Oeste tc else Jogador (x,y) Oeste tc

{- | A função 'pegaLargaCaixa' é a função que faz o jogador pegar ou largar caixas, consoante a sua componente @tc@ e se não
existir nenhum obstáculo que impeça esta interação.
-}
pegaLargaCaixa :: Jogo -> Jogador
pegaLargaCaixa (Jogo mapa (Jogador (x,y) dir tc)) = 
    case tc of False -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) InterageCaixa)) then Jogador (x,y) dir True else Jogador (x,y) dir False
               True  -> if (not (temObstaculo (Jogo mapa (Jogador (x,y) dir tc)) InterageCaixa)) then Jogador (x,y) dir False else Jogador (x,y) dir True

{- | A função 'trocaPecaMapa' é a função que vai trocar a peça @p@ com a peça que está na posição @(x,y)@ do mapa. É útil para
as ações de pegar e largar caixas. 
-}
trocaPecaMapa :: Coordenadas -> Peca -> Mapa -> Mapa
trocaPecaMapa (x,y) p [] = []
trocaPecaMapa (x,y) p (h:t)
   | y == 0 = (trocaPecaLinha x p h) : t
   | otherwise = h : trocaPecaMapa (x,y-1) p t

{- | A função 'trocaPecaLinha' é a função auxiliar da função 'trocaPecaMapa' que troca a peça @p@ pela peça da posição @x@ de
uma linha do mapa.
-}
trocaPecaLinha :: Int -> Peca -> [Peca] -> [Peca]
trocaPecaLinha x  p [] = []
trocaPecaLinha x p (h:t)
   | x == 0 = p : t
   | otherwise = h : trocaPecaLinha (x-1) p t

{- | A função 'atualizaMapa' é uma das funções mais importantes desta tarefa pois é responsável por atualizar o mapa @m@
consoante a posição @c@ do jogador e o movimento @mov@ aplicado ao jogador. Caso o movimento seja 'InterageCaixa' atualiza-se
a posição da caixa com a qual o jogador interagiu. Qualquer outro movimento vai trocar a peça onde o jogador está pela peça Vazio.
Isto deve-se ao inicializar o jogo com umas coordenadas definidas à mão, para que na iteração seguinte não apareça uma peça no 
lugar de onde o jogador começou o jogo.
-}
atualizaMapa :: Mapa -> Jogador -> Movimento -> Mapa 
atualizaMapa m (Jogador c d tc) mov 
      | temObstaculo (Jogo m (Jogador c d tc)) mov = m
      | mov == InterageCaixa = mudaPosCaixa (Jogo m (Jogador c d tc)) InterageCaixa
      | otherwise = trocaPecaMapa c Vazio m

{- | A função 'mudaPosCaixa' vai mudar a posição da caixa com a qual o jogador interage. Se o jogador pegar na caixa em frente
a ele, a caixa simplesmente desaparece do mapa, mas é mostrada gráficamente por cima do jogador. Isto deve-se a que a representação
gráfica de um jogador com a componente @tc@ igual a True esteja definida sempre a aparecer o carater 'C' por cima dele.
-}
mudaPosCaixa :: Jogo -> Movimento -> Mapa
mudaPosCaixa (Jogo mapa (Jogador (x,y) dir tc)) InterageCaixa = 
    case tc of False -> case dir of Este  -> trocaPecaMapa (x+1,y) Vazio mapa
                                    Oeste -> trocaPecaMapa (x-1,y) Vazio mapa
               True  -> largaCaixa (Jogo mapa (Jogador (x,y) dir tc)) InterageCaixa

{- | A função 'largaCaixa' é a função que define onde as coordenadas onde a caixa vai aparecer quando o jogador a largar.
Se à frente do jogador estiver uma peça Vazio tem que se verificar se existem mais peças Vazio por baixo da primeira para
fazer a caixa "cair" nessa abertura do mapa. Caso contrário, a Caixa é colocada na posição acima da peça que se encontra 
à frente dele.
-}                                    
largaCaixa :: Jogo -> Movimento -> Mapa
largaCaixa (Jogo mapa (Jogador (x,y) dir tc)) InterageCaixa = 
    let po = getPecaMapa (x-1,y) mapa
        pe = getPecaMapa (x+1,y) mapa
    in case dir of Este  -> if pe == Vazio then baixaCaixa mapa (x+1,y) else trocaPecaMapa (x+1,y-1) Caixa mapa
                   Oeste -> if po == Vazio then baixaCaixa mapa (x-1,y) else trocaPecaMapa (x-1,y-1) Caixa mapa

{- | A função 'atualizaJogo' é uma função importante, que tem o papel de atualizar o Jogo, isto é, baixar o jogador até ao solo
caso ele se tenha movido para uma posição com peças Vazio por baixo.
-}
atualizaJogo :: Jogo -> Jogo 
atualizaJogo (Jogo m (Jogador (x,y) dir tc)) = Jogo m (baixaJogador m (Jogador (x,y) dir tc))

{- | A função 'baixaJogador' é a função responsável por baixar o jogador para as coordenadas corretas, isto é, para as coordenadas
que ficam imediatamente acima do solo.
-}
baixaJogador :: Mapa -> Jogador -> Jogador
baixaJogador mapa (Jogador c dir tc) = 
    let (a,b) = encontraCoordSolo c mapa
    in Jogador (a,b-1) dir tc
    
{- | A função 'encontraCoordSolo' é a função que vai calcular ordenada da coluna @x@ onde está declarada a primeira peça considerada
como solo, isto é, peças do tipo Bloco e Caixa.
-}
encontraCoordSolo :: Coordenadas -> Mapa -> Coordenadas
encontraCoordSolo (x,y) mapa = if getPecaMapa (x,y) mapa == Bloco || getPecaMapa (x,y) mapa == Caixa then (x,y)
                               else encontraCoordSolo (x,y+1) mapa  

{- | A função 'baixaCaixa' é a função que vai fazer com que a peça largada pelo jogador ignore as peças Vazio por baixo dela e 
"caia" na posição imediatamente acima do solo.
-}
baixaCaixa :: Mapa -> Coordenadas -> Mapa
baixaCaixa mapa (x,y) = let (a,b) = encontraCoordSolo (x,y) mapa
                        in trocaPecaMapa (a,b-1) Caixa mapa


{- | A função 'moveJogador' é a função principal desta Tarefa. Tem o objetivo de alterar o jogo tendo em conta o movimento dado.
Para tal:

* Move, se possível, o jogador segundo esse movimento;
* Atualiza o mapa depois de efetuado o movimento;
* Atualiza o jogo, caso o jogador se tenha colocado numa posição "flutuante"

== Exemplo de utilização:

>>> moveJogador (Jogo [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
                        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                        [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                        [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
                        [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]  (Jogador (6,1) Oeste True) ) AndarEsquerda  
       
       
     CX
P   C<X
XXXXXXX
-}
moveJogador :: Jogo -> Movimento -> Jogo
moveJogador (Jogo m j) movimento = case movimento of 
                                        AndarEsquerda -> atualizaJogo $ Jogo (atualizaMapa m j movimento) (moveEsq (Jogo m j))
                                        AndarDireita -> atualizaJogo $ Jogo (atualizaMapa m j movimento) (moveDir (Jogo m j))
                                        Trepar -> atualizaJogo $ Jogo (atualizaMapa m j movimento) (trepa (Jogo m j))
                                        InterageCaixa -> atualizaJogo $ Jogo (atualizaMapa m j movimento) (pegaLargaCaixa (Jogo m j))            


{- | A função 'correrMovimentos' é a generalização da função 'moveJogador' que vai atualizando o jogo @j@ à medida de vai correndo
os movimentos presentes na lista dada.

== Exemplo de utilização:

>>>correrMovimentos (Jogo [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
                            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                            [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                            [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
                            [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]  
                            (Jogador (6,1) Oeste True) ) 
                            [AndarEsquerda,Trepar,InterageCaixa ] 
       
       
    < X
P  CC X
XXXXXXX
-}
correrMovimentos :: Jogo -> [Movimento] -> Jogo
correrMovimentos j [] = j
correrMovimentos j (h:t) = correrMovimentos (moveJogador j h) t

-- [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]