{- |
Module      : Tarefa3_2021li1g066
Description : Representação textual do jogo
Copyright   : Afonso Marques <a94940@alunos.uminho.pt>;
            : Luís Maia <a95656@alunos.uminho.pt>;

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2021/22.
-}
module Tarefa3_2021li1g066 where

import LI12122

{- | Esta é a intância show principal desta Tarefa. Consiste na representação em string de um 'Jogo'.
Para tal: 

* Converte as peças do mapa @m@ numa string com os carateres correspondentes a cada peça.
* Insere o carater do jogador @jog@ na posição da string correspondente às suas coordenadas.

== Exemplo de utilização:

>>> show (Jogo [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
                 [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                 [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
                 [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
                 [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]]
                 (Jogador (6,0) Oeste False) )
"      <\n      X\n      X\nP   C X\nXXXXXXX"
-}
instance Show Jogo where
  show (Jogo m jog) = insereJogador jog (converteMapa m)

{- | Instância que define o carater de cada tipo de peça
-}
instance Show Peca where
  show (Vazio) = " "
  show (Bloco) = "X"
  show (Caixa) = "C"
  show (Porta) = "P"

{- | A função 'converteMapa' é uma das funções principais para definir a instância Show e consiste na troca das peças do mapa
pelo seu carater correspondente definido anteriormente. 
Para tal, seguiu-se a seguinte estratégia:

* Converter as peças da primeira linha do mapa e concatenar os resultados.
* Caso exista linha seguinte adiciona-se um carater @\n@ no fim das peças da primeira linha
* Constrói-se o resto das linhas do mapa recursivamente, concatenando-se todos os resultados para o resultado final 
ser uma única String  
-}
converteMapa :: Mapa -> String
converteMapa [] = ""
converteMapa (h:t) = foldr (++) "" (map (\x -> show x) h) ++ (if null t then "" else "\n") ++ converteMapa t 

{- | A função 'insereJogador' é outra das funções principais desta Tarefa. Recebe um jogador e um mapa já convertido em String
e insere o jogador nessa string na posição correspondente às suas coordenadas.
Seguiu-se a seguinte estratégia:

1. Separar o mapa em string numa lista com as strings de cada linha;
2. Trocar o carater da string na posição das coordenadas do jogador pelo carater correspondente;
ao jogador, tendo em conta a direção do mesmo;
3. Ter em conta aa componente @tc@ do jogador (se transporta caixa ou não) e decidir se insere um carater de caixa por cima dele;
3. Voltar a concatenar as String numa só.
-}
insereJogador :: Jogador -> String -> String 
insereJogador (Jogador (x,y) dir tc) m 
    | tc == False = insereEnters (trocaElemMapa chr (x,y) (inverteMapaStrToLines m))
    | tc == True =  insereEnters (  trocaElemMapa 'C' (x,y-1) (trocaElemMapa chr (x,y) (inverteMapaStrToLines m)) )
  where chr = averiguaDirecao dir

{- | A função 'averiguaDirecao' é uma função auxiliar que atribui o carater correspondente à direção do jogador dada. 
-}
averiguaDirecao :: Direcao -> Char
averiguaDirecao Este = '>'
averiguaDirecao Oeste = '<'

{- | A função 'insereEnters' é a função que vai concatenar a lista de strings na string final, separando as linhas 
com a string que contém o carater @\n@.
-}
insereEnters :: [String] -> String
insereEnters [] = ""
insereEnters (h:t) = h ++ (if null t then "" else "\n") ++ insereEnters t

{- | A função 'inverteMapaStrToLines' vai receber a string criada na função 'converteMapa' e separa-a nas linhas
usando a função @lines@.
-} 
inverteMapaStrToLines :: String -> [String]
inverteMapaStrToLines [] = [[]]
inverteMapaStrToLines m = lines m

{- | A função 'trocaElemMapa' é a função que vai colocar o carater do jogador na linha e na coluna correspondente, tendo em conta
tanto a direção do jogador e as suas coordenadas.
-}
trocaElemMapa :: Char -> Coordenadas -> [String] -> [String]
trocaElemMapa ch (x,y) [] = []
trocaElemMapa ch (x,y) (h:t)
   | y == 0 = (trocaElemLinha x ch h) : t
   | otherwise = h : trocaElemMapa ch (x,y-1) t

{- | A função 'trocaElemLinha' é a função auxiliar da função 'trocaElemMapa', e serve para trocar o elemento na posição @x@ pelo
carater @ch@ correspondente ao jogador
-}
trocaElemLinha :: Int -> Char -> String -> String
trocaElemLinha x  ch [] = []
trocaElemLinha x ch (h:t)
   | x == 0 = ch : t
   | otherwise = h : trocaElemLinha (x-1) ch t


-- [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],[Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],[Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]  ]