module Tarefa1_2021li1g066_Spec where

import Test.HUnit
import LI12122
import Tarefa1_2021li1g066
import Fixtures

-- Tarefa 1
testsT1 =
  test
    [ "Tarefa 1 - Teste Valida Mapa básico" ~: True  ~=? validaPotencialMapa m1
    
    , "Tarefa 1 - Teste Valida Mapa vazio" ~: validaPotencialMapa [] ~=? False
    
    , "Tarefa 1 - Teste Valida Mapa com 2 portas" ~: validaPotencialMapa [(Porta, (0,0)), (Porta, (1,0))] ~=?  False
    
    , "Tarefa 1 - Teste Valida Mapa com caixas flutuantes" ~: False ~=? validaPotencialMapa mp1
    
    , "Tarefa 1 - Teste Valida Mapa com chão irregular" ~: True ~=? validaPotencialMapa mp2
    
    , "Tarefa 1 - Teste Valida Mapa com mapa muito complexo" ~: True ~=? validaPotencialMapa mp3c
    ]
