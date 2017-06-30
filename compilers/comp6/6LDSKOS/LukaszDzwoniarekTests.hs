-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module LukaszDzwoniarekTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
tests :: [Test]
tests =
  [ Test "t1.pp6"  (SrcFile "t1.pp6") (Eval [42] (Value 43))
  , Test "e2"      (SrcFile "e2.pp6") (Eval [] (Value 43))
  , Test "t0"      (SrcFile "t0.pp6") (Eval [] (Value 6))
  , Test "t1"      (SrcFile "t1.pp6") (Eval [5] (Value 6))
  , Test "t2"      (SrcFile "t2.pp6") (Eval [] (Value 6))
  , Test "t7"      (SrcFile "t7.pp6") (Eval [5] (Value 5))
  , Test "l1"      (SrcFile "l1.pp6") (Eval [] (Value 84))
  , Test "l2_1"    (SrcFile "l2.pp6") (Eval [1] (Value 1))
  , Test "l2_2"    (SrcFile "l2.pp6") (Eval [15] (Value 610))
  , Test "l3"      (SrcFile "l3.pp6") (Eval [2,4] (Value 6))
  , Test "l4_1"    (SrcFile "l4.pp6") (Eval [1,2,3,4] (Value 1))
  , Test "l4_2"    (SrcFile "l4.pp6") (Eval [15,2,3,4] (Value 610))
  , Test "l5_1"    (SrcFile "l5.pp6") (Eval [1,2,3,4] (Value 10))
  , Test "l6_1"    (SrcFile "l6.pp6") (Eval [6] (Value 6))
  , Test "l8_1"    (SrcFile "l8.pp6") (Eval [6] (Value 23))
  , Test "l9_1"    (SrcFile "l9.pp6") (Eval [1,2,3,4] (Value 30))
  ]
