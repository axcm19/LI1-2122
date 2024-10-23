{- |
Module      : Tarefa2_2021li1g066
Description : Construção/Desconstrução do mapa
Copyright   : Afonso Marques <a94940@alunos.uminho.pt>;
            : Luís Maia <a95656@alunos.uminho.pt>;

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2021/22.
-}
module Tarefa2_2021li1g066 where

import LI12122
import Tarefa1_2021li1g066
import Data.List
import Tarefa3_2021li1g066

-- == Funções auxiliares da função 'constroiMapa'

{- | A função 'geraPecasVazio' gera uma lista de peças Vazio com todas as coordenadas possiveis, isto é,
procura na lista de peças dada as maiores componentes x e y e combina-as.
-} 
geraPecasVazio ::  [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
geraPecasVazio m = [(p,(x,y)) | x <- [0..a], y <- [0..b], p <- [Vazio]]
         where a = maiorXmapa m
               b = maiorYmapa m

{- | A função 'removeVazioCoordRep' pega na lista gerada na função 'geraPecasVazio' e remove as peças dessa lista cujas
coordenadas já estejam declaradas na lista de peças dada.
-}
removeVazioCoordRep :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
removeVazioCoordRep [] _ = []
removeVazioCoordRep x [] = x
removeVazioCoordRep (h:t) lp
   | comparaPecas h lp == False = removeVazioCoordRep t lp
   | otherwise = h : removeVazioCoordRep t lp

{- | A função 'juntaVazioLP' junta à lista de peças dada a lista de peças Vazio gerada, já sem a presença de peças 
com coordenadas repetidas.
-}
juntaVazioLP :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
juntaVazioLP lp = lp ++ (removeVazioCoordRep (geraPecasVazio lp) lp)

{- | Depois de se inserir as peças Vazio e as respetivas coordenadas, a função 'separaMatriz' separa as peças nas respetivas
linhas, sendo que a primeira linha da matriz corresponde à primeira lista contida na lista de resultados. Esta função recebe
um valor inteiro @x@ para separar as peças da linha @x@ e recursivamente vai se separando incrementando 1 ao @x@.
-}
separaMatriz :: [(Peca, Coordenadas)] -> Int -> [[(Peca, Coordenadas)]]
separaMatriz [] _ = []
separaMatriz lp x = aux : separaMatriz (lp \\ aux) (x+1)
                     where aux = separaEmLinhas lp x

{- | A função 'separaEmLinhas' seleciona as peças da lista dada que contenham a ordenada igual a @x@, ou seja, todas as que
pertençam a essa linha.
-}
separaEmLinhas :: [(Peca, Coordenadas)] -> Int -> [(Peca, Coordenadas)]
separaEmLinhas [] _ = []
separaEmLinhas ((p,(x,y)):t) b 
   | y == b = (p, (x,y)) : separaEmLinhas t b
   | otherwise = separaEmLinhas t b

{- | A função 'ordenaPecasLinha' ordena as peças de cada linha da lista de peças que foi anteriormente separada em linhas.
Deste modo, cada peça fica no exato local da matriz correspondente às suas coordenadas.
-}
ordenaPecasLinha :: [[(Peca, Coordenadas)]] -> [[(Peca, Coordenadas)]]
ordenaPecasLinha [] = []
ordenaPecasLinha (h:t) = ordena h : ordenaPecasLinha t

{- | A função 'insere' é uma função auxiliar que insere uma peça numa lista de peças já ordenada pelas abcissas
-}
insere :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
insere x [] = [x]
insere (p,c) l@((p1,c1):t)
   | c < c1 = (p,c) : l
   | otherwise = (p1,c1) : insere (p,c) t

{- | A função 'ordena' é uma função auxiliar que insere a primeira peça da lista na restante parte da lista ordenada recursivamente.
-}
ordena :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
ordena [] = []
ordena (h:t) = insere h (ordena t)

{- | A função auxiliar 'removeCoordenadasLinha' faz desaparecer as coordenadas relativas às peças de uma linha da matriz, pois
estas já se encontram nas suas posições corretas.
-}
removeCoordenadasLinha :: [(Peca, Coordenadas)] -> [Peca]
removeCoordenadasLinha [] = []
removeCoordenadasLinha ((p,c):t) = p : removeCoordenadasLinha t

{- | Uma vez que as peças já estão nos seus respetivos lugares, é preciso remover as coordenadas das peças. A função 
'semCoordenadas' trata exatamente de ir removendo linha a linha as coordenadas das peças.
-}
semCoordenadas :: [[(Peca, Coordenadas)]] -> Mapa
semCoordenadas [] = []
semCoordenadas (h:t) = removeCoordenadasLinha h : semCoordenadas t

{- | A função 'constroiMapa' é uma das funções principais desta tarefa. Consiste em construir um Mapa através de uma lista
de pares (peça, coordenada). Para tal recebe uma lista de peças e segue a seguinte estratégia:

1. Junta peças Vazio que não estão declaradas à lista dada;
2. Separa a lista nas respetivas linhas, recebendo o @0@ como valor inicializador pois a primeira linha encontra-se na posiçao 0;
3. Ordena as peças de cada linha por ordem crescente das abcissas;
4. Retira as coordenadas, ficando exatamente com um Mapa.

== Exemplo de utilização:

>>> constroiMapa [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),
                  (Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),
                  (Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),
                  (Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]
[[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
 [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Bloco],
 [Porta,Vazio,Vazio,Vazio,Caixa,Vazio,Bloco],
 [Bloco,Bloco,Bloco,Bloco,Bloco,Bloco,Bloco]]

-}
constroiMapa :: [(Peca, Coordenadas)] -> Mapa
constroiMapa [] = []
constroiMapa lp = semCoordenadas (ordenaPecasLinha (separaMatriz (juntaVazioLP lp) 0))

-- == Funções auxiliares da função 'desconstroiMapa'

{- | A função 'desconstroiMapa' é uma das funções principais desta tarefa. Consiste em desconstruir um Mapa retornando uma lista
de pares (peça, coordenada). Para tal recebe um mapa segue a seguinte estratégia:

1. Insere as coordenadas a cada uma das peças, inicializando com as coordenadas (0,0);
2. Remove as declarações de peças Vazio e das respetivas coordenadas, sendo o resultado final o objetivo desta função.

== Exemplo de utilização:

>>> desconstroiMapa [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                     [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                     [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
                     [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
[(Bloco,(6,0)),(Bloco,(6,1)),(Porta,(0,2)),
 (Caixa,(4,2)),(Bloco,(6,2)),(Bloco,(0,3)),
 (Bloco,(1,3)),(Bloco,(2,3)),(Bloco,(3,3)),
 (Bloco,(4,3)),(Bloco,(5,3)),(Bloco,(6,3))]
-}
desconstroiMapa :: Mapa -> [(Peca, Coordenadas)]
desconstroiMapa mapa = eliminaVazioLista (insereCoordenadas mapa 0 0)

{- A função 'insereCoordenadas' insere as coordenadas a todas as peças de todas linhas da matriz, concatenando os resultados.
-}
insereCoordenadas :: Mapa -> Int -> Int -> [(Peca, Coordenadas)]
insereCoordenadas [] _ _ = []
insereCoordenadas (l1 : resto) x y = (insereCoordenadasLinha l1 x y) ++ insereCoordenadas resto x (y+1)  

{- A função auxiliar 'insereCoordenadasLinha' insere as coordenadas a uma lista de peças, correspondente a uma linha da matriz.
-}
insereCoordenadasLinha :: [Peca] -> Int -> Int -> [(Peca, Coordenadas)]
insereCoordenadasLinha [] _ _ = []
insereCoordenadasLinha (h:t) x y = (h, (x,y)) : insereCoordenadasLinha t (x+1) y


{- A função 'eliminaVazioLista' remove os pares de peças vazias e respetivas coordenadas, pois estas não são necessárias para
a declaração de listas de peças/coordenadas.
-}
eliminaVazioLista :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
eliminaVazioLista [] = []
eliminaVazioLista ((h,x):t) 
   | h == Vazio = eliminaVazioLista t
   | otherwise = (h,x) : eliminaVazioLista t

-- [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
-- [(Porta, (0,2)), (Bloco, (0,3)), (Bloco, (1,3)),(Bloco, (2,3)), (Bloco, (3,3)), (Caixa, (4,2)),(Bloco, (4,3)), (Bloco, (5,3)), (Bloco, (6,0)),(Bloco, (6,1)), (Bloco, (6,2)), (Bloco, (6,3))]