{-# LANGUAGE Safe #-}

module AdamSawickiTests(tests) where

import DataTypes

--Zawiera poprawione testy z poprzedniego zadania

tests :: [Test]
tests =
  [ Test "test_plik19" (SrcFile "test_plik19.pp6") (Eval [0] (Value 0))
  , Test "test_plik20" (SrcFile "test_plik20.pp6") (Eval [0] (Value 0))
  , Test "test_plik21" (SrcFile "test_plik21.pp6") (Eval [0] (Value 0))
  ]
