{- |
Module      : Tarefa1_2021li1g066
Description : Validação de um potencial mapa
Copyright   : Afonso Marques <a94940@alunos.uminho.pt>;
            : Luís Maia <a95656@alunos.uminho.pt>;

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2021/22.
-}
module Tarefa1_2021li1g066 where

import LI12122
import Data.List
import Tarefa3_2021li1g066


{- | A função 'maiorYmapa' calcula o número de linhas da matriz recorrendo ao método de descobrir o maior y da lista de peças. 
-}
maiorYmapa :: [(Peca, Coordenadas)] -> Int
maiorYmapa l = aux l 0
   where aux [] acc = acc
         aux ((_, (x,y)):t) acc = aux t (max y acc)

{- | A função 'linhasM' calcula o número de linhas da matriz somando 1 à maior ordenada 
da lista de peças dada. Isto deve-se a existirem coordenadas com o valor 0.
-}
linhasM :: [(Peca, Coordenadas)] -> Int
linhasM [] = 0
linhasM pecas = maiorYmapa pecas + 1               

{- | A função 'maiorXmapa' calcula o número de colunas da matriz, recorrendo ao método de descobrir o maior x da lista de peças.
-}
maiorXmapa :: [(Peca, Coordenadas)] -> Int
maiorXmapa l = aux l 0
   where aux [] acc = acc
         aux ((_, (x,y)):t) acc = aux t (max x acc)


{- | A função 'colunasM' calcula o número de colunas da matriz somando 1 à maior abcissa da lista de peças dada.
-}
colunasM :: [(Peca, Coordenadas)] -> Int
colunasM [] = 0
colunasM pecas = maiorXmapa pecas + 1 


{- | A função 'getPeca' é uma função auxiliar muito útil que recebe umas coordenadas @(x,y)@ e uma lista de pecas e 
seleciona e retorna a peça com essas coordenadas. A peça final é colocada numa lista para efeitos de recursividade.
-}
getPeca :: Coordenadas -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
getPeca _ [] = []
getPeca (x,y) ((p,(a,b)):t)
   | (x,y) == (a,b) = [(p,(a,b))]
   | otherwise = getPeca (x,y) t

{- | A função 'getPecasAdj' junta numa lista as peças acima, abaixo e à direita da peça dada. Serve para mais adiante se
verificar se a peça dada está junta a uma peca Bloco, determinando a continuidade do chão.
-}
getPecasAdj :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
getPecasAdj _ [] = []
getPecasAdj (p,(x,y)) lp = (getPeca (x+1,y) lp) ++ (getPeca (x,y-1) lp) ++ (getPeca (x,y+1) lp)

{- | A função 'procUltPecasColunas' junta numa lista as últimas peças de cada coluna. Serve para futuramente se verificar 
a validade do chão.
-}
procUltPecasColunas :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
procUltPecasColunas _ [] = []
procUltPecasColunas cl l = let a = filtraColunaX cl l
                               b = getPeca (cl, maiorYmapa a) a
                           in b ++ (procUltPecasColunas (cl+1) (l \\ a))

{- | A função 'filtraColunaX' é a função auxiliar da função 'procUltPecasColunas', que dado um inteiro @n@ vai filtrar a lista
de peças dada e retornar apenas as peças que pertençam à coluna @n@.
-}
filtraColunaX :: Int -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
filtraColunaX _ [] = []
filtraColunaX n ((p,(x,y)):t)
   | n == x = (p,(x,y)) : filtraColunaX n t
   | otherwise = filtraColunaX n t

{- | A função 'verPecasAdj' pega na lista de últimas peças das colunas e no mapa e constrói uma lista com a lista
de peças adjacentes respetivamente.
-}
verPecasAdj :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [[(Peca, Coordenadas)]]
verPecasAdj [] _ = []
verPecasAdj (h:t) lp = getPecasAdj h lp : verPecasAdj t lp

{- | A função 'checkTamanhos' verifica se o tamanho da lista de peças adjacentes é pelo menos 1, para assegurar que
todas as últimas peças Bloco estão ligadas a outra peça Bloco.
-}
checkTamanhos :: [[(Peca, Coordenadas)]] -> [Bool]
checkTamanhos [] = []
checkTamanhos (h:t)
   | length h >=1 = True : checkTamanhos t
   | otherwise = [False]


{- | A função 'chaoOk' é uma auxiliar da função principal que valida o chão do mapa. 
Para tal é necessário que haja continuidade das peças Bloco.
Para resolver este problema seguimos a seguinte estratégia :

* Juntar numa lista as últimas peças de cada coluna do mapa, da esquerda para a direita
* Verificar quais as peças que estão acima, abaixo e à direita de cada peça da lista
do ponto anterior. Para haver continuidade no chão, os blocos têm que estar ligados 
por uma dessas posições.
* Para que haja continuidade, tem que haver pelo menos uma peça adjacente, por isso o tamanho
da lista de peças adjacentes tem que ser maior que 1
* Verificar se todas as peças finais da coluna têm uma ou mais peça adjacente
* No fim, fazer a conjunção dos resultados, validando ou não o chão do mapa
-}
chaoOk :: [(Peca, Coordenadas)] -> Bool 
chaoOk p = if (length alpha) < (colunasM p) then False
           else and alpha
    where alpha =(checkTamanhos (verPecasAdj (procUltPecasColunas 0 p) p))  

{- | A função 'contaPorta' conta quantas portas existem no mapa. Para validação tem de ser contada apenas uma porta
-}
contaPorta :: [(Peca, Coordenadas)] -> Int
contaPorta [] = 0
contaPorta ((x,y):t) = case x of
                        Porta -> 1 + contaPorta t
                        _ -> 0 + contaPorta t

{- | A função 'portasOk' é uma auxiliar da função principal que confirma a validade do número de portas.
Se apenas existir 1 retorna True, caso contrário retorna False.
-}
portasOk :: [(Peca, Coordenadas)] -> Bool
portasOk pecas = let x = contaPorta pecas  
                 in x == 1 

{- | A função 'comparaPecas' recebe uma peça e verifica se existe alguma com coordenadas iguais numa lista de peças. 
Havendo coordenadas iguais não é valido e retorna False. Caso contrário é válido e retorna True.
-}
comparaPecas :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Bool
comparaPecas _ [] = True
comparaPecas (p1,c1) ((p2,c2):t) 
     | c1 == c2 = False
     | c1 /= c2 = True && comparaPecas (p1,c1) t

{- | A função 'pecasOk' é uma auxiliar da função principal. Verifica se exitem peças com coordenadas iguais. 
Caso existam, não é valido e retorna False. Caso contrário é válido e retorna True. 
-}
pecasOk :: [(Peca, Coordenadas)] -> Bool
pecasOk [] = True
pecasOk (h:t) = (comparaPecas h t) && pecasOk t


{- | A função 'getPecaInferior' retorna a peça inferior à peça dada. O resultado é inserido numa lista para ser possível
retornar um resultado no caso de paragem da recursividade e também para futuramente poder ser comparável.
-}
getPecaInferior :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
getPecaInferior _ [] = []
getPecaInferior (p, (x,y)) ((p1,c1):t) 
   | c1 == (x,y+1) = [(p1,c1)]
   | otherwise = getPecaInferior (p, (x,y)) t

{- | A função 'caixasOk' é uma função auxiliar da principal. Verifica se as caixas estão bem posicionadas, ou seja, 
se estão em cima de outra caixa ou bloco. Para tal foi necessário separar as caixas do resto das peças, verificar a 
validade de cada uma, juntando os respetivos resultados numa lista e fazendo a conjunção desses resultados.
-}
caixasOk :: [(Peca, Coordenadas)] -> Bool
caixasOk [] = True
caixasOk l = and (validadeCaixas cx l)
              where cx = juntaCaixas l

{- | A função 'juntaCaixas' junta todas as caixas de uma lista de peças numa nova lista para mais tarde verificar as 
respetivas peças inferiores.
-}
juntaCaixas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)]
juntaCaixas [] = []
juntaCaixas (h:t)
   | fst h == Caixa = h : juntaCaixas t
   | otherwise = juntaCaixas t

{- | A função 'validadeCaixas' verifica a validade das caixas e utiliza a função 'verificaPecaInferior', 
guardando os respetivos resultados numa lista, para
mais tarde se fazer a conjunção desses resultados.
-}
validadeCaixas :: [(Peca, Coordenadas)] -> [(Peca, Coordenadas)] -> [Bool]
validadeCaixas [] _ = []
validadeCaixas (h:t) lc = verificaPecaInferior h lc : validadeCaixas t lc


{- | A função 'verificaPecaInferior' verifica a validade da peça inferior de uma caixa.
É necessária para a validação das caixas. 
-}
verificaPecaInferior :: (Peca, Coordenadas) -> [(Peca, Coordenadas)] -> Bool
verificaPecaInferior _ [] = True
verificaPecaInferior (p ,(x,y)) l = let alpha = getPecaInferior (p, (x,y)) l 
                           in case alpha of [(Bloco,c)] -> True
                                            [(Caixa,c)] -> True
                                            []      -> False 

{- | A função 'contaVazio' é auxiliar da função 'vazioOk' que conta o número de peças Vazio. 
Para tal, calcula o número total de peças e retira o número de peças descritas na lista de peças. 
O resultado será o número de peças vazias.
-}
contaVazio :: [(Peca, Coordenadas)] -> Int
contaVazio lp = let (c,l) = (colunasM lp, linhasM lp)
                    --l = linhasM lp
                 in (c*l) - (length lp)

{- | A função 'vazioOk' é uma das principais funções auxiliares da função principal desta tarefa.
Verifica se existe pelo menos uma peça Vazio na lista de peças.
-}
vazioOk :: [(Peca, Coordenadas)] -> Bool
vazioOk lp = contaVazio lp > 0

{- | A função 'validaPotencialMapa' é a função principal desta tarefa. Consiste em verificar se o mapa @l@ 
corresponde a uma mapa válido, obedecendo às 5 condições do enunciado.

== Exemplo de utilização:

>>> validaPotencialMapa [(Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),
                         (Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),
                         (Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),
                         (Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]
True
-}
validaPotencialMapa :: [(Peca, Coordenadas)] -> Bool
validaPotencialMapa [] = False
validaPotencialMapa l = pecasOk l && portasOk l && caixasOk l && vazioOk l && chaoOk l

-- t1 = [ (Porta, (0, 3)),(Bloco, (0, 4)),(Bloco, (1, 4)),(Bloco, (2, 4)),(Bloco, (3, 4)),(Bloco, (4, 4)),(Caixa, (4, 3)),(Bloco, (5, 4)),(Bloco, (6, 4)),(Bloco, (6, 3)),(Bloco, (6, 2)),(Bloco, (6, 1))]