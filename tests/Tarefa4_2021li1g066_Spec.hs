module Tarefa4_2021li1g066_Spec where

import Test.HUnit
import LI12122
import Tarefa3_2021li1g066
import Tarefa4_2021li1g066
import Fixtures

testsT4 =
  test
    [ "Tarefa 4 - Teste Move m1e1 Oeste e cair num precipício" ~: Jogo m1r (Jogador (5, 3) Oeste False) ~=?  moveJogador m1e1 AndarEsquerda
    
    , "Tarefa 4 - Teste tenta mover Jogador para fora do mapa" ~: Jogo m1r (Jogador (6, 0) Este False) ~=?  moveJogador m1e1 AndarDireita
    
    , "Tarefa 4 - Teste Trepar" ~: m1e1 ~=? moveJogador m1e1 Trepar
    
    , "Tarefa 4 - Teste InterageCaixa sem caixa à sua frente" ~: m1e1 ~=?  moveJogador m1e1 InterageCaixa
    
    , "Tarefa 4 - Teste movimentos m1e1" ~: m1e2 ~=?  correrMovimentos m1e1 [AndarEsquerda, Trepar, AndarEsquerda, AndarEsquerda]
    
    , "Tarefa 4 - Teste movimentos m1e2 Caixa1" ~: Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (3, 3) Este True) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa]
    
    , "Tarefa 4 - Teste movimentos m1e2 Caixa2" ~:
      Jogo
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Caixa, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ]
        (Jogador (2, 3) Oeste False) ~=?  correrMovimentos m1e2 [AndarDireita, InterageCaixa, AndarEsquerda, InterageCaixa]
    
    , "Tarefa 4 - Teste move jogador com caixa e cai num precipício" ~: Jogo m1r2 (Jogador (5,3) Oeste True) ~=? moveJogador m1e3 AndarEsquerda
    
    , "Tarefa 4 - Teste Trepar com caixa" ~: Jogo m1r (Jogador (4,2) Este True) ~=? moveJogador (Jogo m1r (Jogador (3,3) Este True)) Trepar
    
    , "Tarefa 4 - Teste tentar trepar com caixa para local com obstáculo impeditivo" ~: Jogo m1rv2 (Jogador (3,3) Este True) ~=? moveJogador (Jogo m1rv2 (Jogador (3,3) Este True)) Trepar
    
    , "Tarefa 4 - Teste tentar trepar sem caixa para local com obstáculo impeditivo" ~: Jogo m1rv2 (Jogador (3,3) Este False) ~=? moveJogador (Jogo m1rv2 (Jogador (3,3) Este False)) Trepar
    
    , "Tarefa 4 - Teste InterageCaixa com caixa à sua frente" ~: Jogo 
        [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Porta, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco]
        , [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
        ] 
        (Jogador (3,3) Este True) ~=? moveJogador (Jogo m1r (Jogador (3,3) Este False)) InterageCaixa
    
    , "Tarefa 4 - Teste InterageCaixa com obstáculo impeditivo" ~: Jogo m1rv2 (Jogador (3,3) Este False) ~=? moveJogador (Jogo m1rv2 (Jogador (3,3) Este False)) InterageCaixa
    
    , "Tarefa 4 - Teste largar caixa simples" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
         [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
         [Vazio, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
         [Porta, Vazio, Vazio, Vazio, Caixa, Vazio, Bloco],
         [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
         ]
         (Jogador (3,3) Este False) ~=? moveJogador (Jogo m1r2 (Jogador (3,3) Este True)) InterageCaixa
    
    , "Tarefa 4 - Teste largar caixa num precipício" ~: Jogo 
        [[Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
         [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
         [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Bloco],
         [Porta, Vazio, Vazio, Vazio, Caixa, Caixa, Bloco],
         [Bloco, Bloco, Bloco, Bloco, Bloco, Bloco, Bloco]
         ]
         (Jogador (6,1) Oeste False) ~=? moveJogador m1e3 InterageCaixa
    ]