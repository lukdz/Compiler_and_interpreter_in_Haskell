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
  [ Test "inc"      (SrcString "input x in (fn(y:int) -> y+x+1) x") (Eval [2] (Value 5))
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "1"        (SrcString "(fn(y:int) -> y) 1")   (Eval [] (Value 1))
  , Test "2"        (SrcString "(fn(y:int) -> y+1) 1")   (Eval [] (Value 2))
  , Test "x=3"      (SrcString "input x in (fn(y:int) -> y+x) x") (Eval [3] (Value 6))
  , Test "1+1"      (SrcString "(fn(y:unit) -> 1+1) ()")   (Eval [] (Value 2))
  , Test "math-a"    (SrcString "input x y in (fn(x:int) -> x) x") (Eval [10,5] (Value 10))
  , Test "math-b"    (SrcString "input x y in (fn(x:int) -> y) x") (Eval [10,5] (Value 5))
  , Test "math-c"    (SrcString "input x y in (fn(x:int*int) -> snd x + fst x) (x,y)") (Eval [10,5] (Value 15))
  , Test "math1"    (SrcString "input x y in (fn(x:int*int) -> y + fst x) (x,y)") (Eval [10,5] (Value 15))
  , Test "math2"    (SrcString "input x y in (fn(x:int*int) -> fst x - y) (x,y)") (Eval [10,5] (Value 5))
  , Test "math3"    (SrcString "input x y z in (fn(x:int*int) -> fst x - y - z) (x,y)") (Eval [10,5,3] (Value 2))
  , Test "math4"    (SrcString "input x y in (fn(x:int*int) -> y * fst x) (x,y)") (Eval [10,5] (Value 50))
  , Test "math5"    (SrcString "input x y z in (fn(x:int*int) -> y * fst x * z) (x,y)") (Eval [10,5,3] (Value 150))
  , Test "math6_0"  (SrcString "(fn(x:int*int) -> 10 div (fn(y:int) -> y) 5) (2)") TypeError
  , Test "math6_1"  (SrcString "input x y in x div (fn(y:int) -> -y) (-y)") (Eval [10,5] (Value 2))
  , Test "math7"    (SrcString "input x y z in x div y div (fn(y:int) -> y) z") (Eval [125,5,5] (Value 5))
  , Test "math8"    (SrcString "input x y z in (x div (fn(y:int) -> y) y) + z") (Eval [15,5, 2] (Value 5))
  , Test "math9"    (SrcString "input x y z in (x * (fn(y:int) -> y) y) - (fn(y:int) -> -y) (- z)")   (Eval [7, 5, 3] (Value 32))
  , Test "math10"   (SrcString "input x in (fn(y:int) -> -y) x")   (Eval [-5] (Value 5))
  , Test "math11"   (SrcString "input x y in x + (fn(y:int) -> -y) y")   (Eval [10,5] (Value 5))
  , Test "math12"   (SrcString "input x in (fn(y:int) -> -y) x")   (Eval [5] (Value (-5)))

  , Test "bool0"    (SrcString "if (fn(x:unit) -> 2 < 5) () then 1 else 0")   (Eval [] (Value 1))
  , Test "bool1"    (SrcString "input x in if (fn(u:unit) -> x < x) () then 1 else 0")   (Eval [1] (Value 0))
  , Test "bool2"    (SrcString "input x y in if (fn(u:unit) -> x < y) () then 1 else 0")   (Eval [10,5] (Value 0))
  , Test "bool3"    (SrcString "input x y in if (fn(u:unit) -> not x < y) () then 1 else 0 ")   (Eval [10,5] (Value 1))
  , Test "bool4"    (SrcString "input x y in if (fn(u:unit) -> x < y or x > y) () then 1 else 0")   (Eval [10,5] (Value 1))
  , Test "bool5"    (SrcString "input x y in if (fn(u:unit) -> x < y) () or x > y and x < y then 1 else 0")   (Eval [10,5] (Value 0))
  , Test "bool6"    (SrcString "input x in if (fn(y:int) -> -y) x < (fn(y:int) -> y)5 then 1 else 0")   (Eval [1] (Value 1))
  , Test "bool7"    (SrcString "input x in x < (fn(y:int) -> y) 5")   TypeError
  , Test "bool8"    (SrcString "(fn(u:unit) -> 1 < 5) ()")   TypeError

  , Test "if1"      (SrcString "input x y in if x < y then 1  else (fn(y:int) -> y) 0")   (Eval [2, 5] (Value 1))
  , Test "if2"      (SrcString "input x y in if x < y then (fn(y:int) -> y) 1  else 0")   (Eval [2, 5] (Value 1))

  , Test "let2"     (SrcString "input x in let y = (fn(y:int) -> y) x in x + y")     (Eval [2] (Value 4))

  , Test "letbool1" (SrcString "if (fn(u:unit) -> 1<2) () then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool2" (SrcString "if (fn(u:unit) -> false) () then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool3" (SrcString "let x = (fn(u:unit) -> true) () in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool4" (SrcString "let x = (fn(u:unit) -> (if true then true else false)) () in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool5" (SrcString "let x = (fn(u:unit) -> (if 1<2 then true else false))  () in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool6" (SrcString "let x = (fn(u:unit) -> (if 1<2 then 1<2  else 1<2))    () in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool7" (SrcString "let x = (fn(u:unit) -> (if 1<2 then 1<2  else 1<2))    () in if x then 1<2 else 1<2") TypeError

  , Test "test_pl0_1" (SrcFile "LukaszDzwoniarekTests/test_pl0.pp6") (Eval [] (Value 4))
  , Test "test_pl1_1" (SrcFile "LukaszDzwoniarekTests/test_pl1.pp6") (Eval [1,2, 3, 4] (Value 4))
  , Test "test_pl2_1" (SrcFile "LukaszDzwoniarekTests/test_pl2.pp6") (Eval [0] RuntimeError)
  , Test "test_pl5_1" (SrcFile "LukaszDzwoniarekTests/test_pl5.pp6") (Eval [1] (Value 1))
  , Test "test_pl5_2" (SrcFile "LukaszDzwoniarekTests/test_pl5.pp6") (Eval [2] (Value 1))
  , Test "test_pl6_1" (SrcFile "LukaszDzwoniarekTests/test_pl6.pp6") (Eval [2] (Value 2))
  , Test "test_pl6_2" (SrcFile "LukaszDzwoniarekTests/test_pl6.pp6") (Eval [777] (Value 777))


  , Test "test_func0_1" (SrcFile "LukaszDzwoniarekTests/test_func0.pp6") (Eval [1] (Value 1))
  , Test "test_func1_1" (SrcFile "LukaszDzwoniarekTests/test_func1.pp6") (Eval [3] (Value 3))

  , Test "test_list0_1" (SrcFile "LukaszDzwoniarekTests/test_list0.pp6") (Eval [] (Value 2))
  , Test "test_list1_1" (SrcFile "LukaszDzwoniarekTests/test_list1.pp6") (Eval [] (Value 2))
  , Test "test_list2_1" (SrcFile "LukaszDzwoniarekTests/test_list2.pp6") (Eval [] (Value 10))
  , Test "test_list3_1" (SrcFile "LukaszDzwoniarekTests/test_list3.pp6") TypeError

  , Test "fun1"    (SrcString "fun f(x:int):int = x input x in x + 1") (Eval [42] (Value 43))
  , Test "t1.pp6"  (SrcFile "LukaszDzwoniarekTests/t1.pp6") (Eval [42] (Value 43))
  , Test "e1"      (SrcFile "LukaszDzwoniarekTests/e1.pp6") TypeError
  , Test "e2"      (SrcFile "LukaszDzwoniarekTests/e2.pp6") (Eval [] (Value 43))
  , Test "t0"      (SrcFile "LukaszDzwoniarekTests/t0.pp6") (Eval [] (Value 6))
  , Test "t1"      (SrcFile "LukaszDzwoniarekTests/t1.pp6") (Eval [5] (Value 6))
  , Test "t2"      (SrcFile "LukaszDzwoniarekTests/t2.pp6") (Eval [] (Value 6))
  , Test "t3"      (SrcFile "LukaszDzwoniarekTests/t3.pp6") TypeError
  , Test "t7"      (SrcFile "LukaszDzwoniarekTests/t7.pp6") (Eval [5] (Value 5))
  , Test "l1"      (SrcFile "LukaszDzwoniarekTests/l1.pp6") (Eval [] (Value 84))
  , Test "l2_1"    (SrcFile "LukaszDzwoniarekTests/l2.pp6") (Eval [1] (Value 1))
  , Test "l2_2"    (SrcFile "LukaszDzwoniarekTests/l2.pp6") (Eval [15] (Value 610))
  , Test "l3"      (SrcFile "LukaszDzwoniarekTests/l3.pp6") (Eval [2,4] (Value 6))
  , Test "l4_1"    (SrcFile "LukaszDzwoniarekTests/l4.pp6") (Eval [1,2,3,4] (Value 1))
  , Test "l4_2"    (SrcFile "LukaszDzwoniarekTests/l4.pp6") (Eval [15,2,3,4] (Value 610))
  , Test "l5_1"    (SrcFile "LukaszDzwoniarekTests/l5.pp6") (Eval [1,2,3,4] (Value 10))
  , Test "l6_1"    (SrcFile "LukaszDzwoniarekTests/l6.pp6") (Eval [6] (Value 6))
  , Test "l7_1"    (SrcFile "LukaszDzwoniarekTests/l7.pp6") TypeError
  , Test "l8_1"    (SrcFile "LukaszDzwoniarekTests/l8.pp6") (Eval [6] (Value 23))
  , Test "l9_1"    (SrcFile "LukaszDzwoniarekTests/l9.pp6") (Eval [1,2,3,4] (Value 30))
  ]
