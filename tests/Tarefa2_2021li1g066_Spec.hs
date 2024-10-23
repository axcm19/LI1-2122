module Tarefa2_2021li1g066_Spec where

import Data.List (sort)
import Test.HUnit
import LI12122
import Tarefa2_2021li1g066
import Fixtures

testsT2 =
  test
    [ "Tarefa 2 - Teste Construir Mapa m1" ~: m1r ~=? constroiMapa m1
    
    , "Tarefa 2 - Teste Construir Mapa vazio" ~: [] ~=? constroiMapa []
    
    , "Tarefa 2 - Teste Desconstruir Mapa m1" ~: sort m1 ~=?  sort (desconstroiMapa m1r)
    
    , "Tarefa 2 - Teste Desconstruir Mapa vazio" ~: [] ~=? desconstroiMapa []
    
    , "Tarefa 2 - Teste Identidade m1" ~: sort m1 ~=?  sort (desconstroiMapa (constroiMapa m1))
    
    , "Tarefa 2 - Teste Identidade m1r" ~: m1r ~=?  constroiMapa (desconstroiMapa m1r)
    
    , "Tarefa 2 - Teste Construir Mapa complexo mp3c" ~: mp3cm ~=? constroiMapa mp3c
    
    , "Tarefa 2 - Teste Desconstruir Mapa complexo mp3cm" ~: sort mp3c ~=? sort (desconstroiMapa mp3cm)
    ]