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
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [ Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "1"        (SrcString "1")   (Eval [] (Value 1))
  , Test "2"        (SrcString "2")   (Eval [] (Value 2))
  , Test "x=3"      (SrcString "input x in x") (Eval [3] (Value 3))
  --, Test "x!=3 FAIL"      (SrcString "input x in x") (Eval [3] (Value 5))
  , Test "1+1"        (SrcString "1+1")   (Eval [] (Value 2))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "math1"    (SrcString "input x y in x + y") (Eval [10,5] (Value 15))
  , Test "math2"    (SrcString "input x y in x - y") (Eval [10,5] (Value 5))
  , Test "math3"    (SrcString "input x y z in x - y - z") (Eval [10,5,3] (Value 2))
  , Test "math4"    (SrcString "input x y in x * y") (Eval [10,5] (Value 50))
  , Test "math5"    (SrcString "input x y z in x * y * z") (Eval [10,5,3] (Value 150))
  , Test "math60"    (SrcString "10 div 5") (Eval [10,5] (Value 2))
  , Test "math61"    (SrcString "input x y in x div y") (Eval [10,5] (Value 2))
  , Test "math7"    (SrcString "input x y z in x div y div z") (Eval [125,5,5] (Value 5))
  , Test "math8"    (SrcString "input x y z in (x div y) + z") (Eval [15,5, 2] (Value 5))
  , Test "math9"    (SrcString "input x y z in (x * y) - z")   (Eval [7, 5, 3] (Value 32))
  , Test "math10"   (SrcString "input x in - x")   (Eval [-5] (Value 5))
  , Test "math11"   (SrcString "input x y in x + - y")   (Eval [10,5] (Value 5))
  , Test "math12"   (SrcString "input x in - x")   (Eval [5] (Value (-5)))

  , Test "bool0"    (SrcString "if 2 < 5 then 1 else 0")   (Eval [] (Value 1))
  --, Test "bool0 FAIL"    (SrcString "if 2 < 5 then 1 else 0")   (Eval [] (Value 0))
  , Test "bool1"    (SrcString "input x in if x < 5 then 1 else 0")   (Eval [1] (Value 1))
  --, Test "bool1 FAIL"    (SrcString "input x in if x < 5 then 1 else 0")   (Eval [1] (Value 0))
  , Test "bool2"    (SrcString "input x y in if x < y then 1 else 0")   (Eval [10,5] (Value 0))
  , Test "bool3"    (SrcString "input x y in if not x < y then 1 else 0 ")   (Eval [10,5] (Value 1))
  , Test "bool4"    (SrcString "input x y in if x < y or x > y then 1 else 0")   (Eval [10,5] (Value 1))
  , Test "bool5"    (SrcString "input x y in if x < y or x > y and x < y then 1 else 0")   (Eval [10,5] (Value 0))
  , Test "bool6"    (SrcString "input x in if - x < 5 then 1 else 0")   (Eval [1] (Value 1))
  , Test "bool7"    (SrcString "input x in x < 5")   TypeError
  , Test "bool8"    (SrcString "1 < 5")   TypeError

  , Test "if1"      (SrcString "input x y in if x < y then 1  else 0")   (Eval [2, 5] (Value 1))
  , Test "if2"      (SrcString "input x y in if x < y then 1  else 0")   (Eval [2, 5] (Value 1))
  --, Test "if3"      (SrcString "input x y in if x < y then 1")   TypeError

  --, Test "let1"     (SrcString "input x in let true = x in x + 1")                TypeError
  , Test "let2"     (SrcString "input x in let y = x in x + y")     (Eval [2] (Value 4))

  , Test "letbool1" (SrcString "if 1<2 then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool2" (SrcString "if false then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool3" (SrcString "let x = true in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool4" (SrcString "let x = (if true then true else false) in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool5" (SrcString "let x = (if 1<2 then true else false) in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool6" (SrcString "let x = (if 1<2 then 1<2 else 1<2) in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool7" (SrcString "let x = (if 1<2 then 1<2 else 1<2) in if x then 1<2 else 1<2") TypeError


  , Test "test_pl0_1" (SrcFile "LukaszDzwoniarekTests/test_pl0.pp5") (Eval [] (Value 4))
  , Test "test_pl1_1" (SrcFile "LukaszDzwoniarekTests/test_pl1.pp5") (Eval [1,2, 3, 4] (Value 4))
  , Test "test_pl2_1" (SrcFile "LukaszDzwoniarekTests/test_pl2.pp5") (Eval [0] RuntimeError)
  , Test "test_pl5_1" (SrcFile "LukaszDzwoniarekTests/test_pl5.pp5") (Eval [1] (Value 1))
  , Test "test_pl5_2" (SrcFile "LukaszDzwoniarekTests/test_pl5.pp5") (Eval [2] (Value 1))

  , Test "test_func0_1" (SrcFile "LukaszDzwoniarekTests/test_func0.pp5") (Eval [1] (Value 1))
  , Test "test_func1_1" (SrcFile "LukaszDzwoniarekTests/test_func1.pp5") (Eval [3] (Value 3))

  , Test "test_list0_1" (SrcFile "LukaszDzwoniarekTests/test_list0.pp5") (Eval [] (Value 2))
  , Test "test_list1_1" (SrcFile "LukaszDzwoniarekTests/test_list1.pp5") (Eval [] (Value 2))
  , Test "test_list2_1" (SrcFile "LukaszDzwoniarekTests/test_list2.pp5") (Eval [] (Value 10))


  ]
