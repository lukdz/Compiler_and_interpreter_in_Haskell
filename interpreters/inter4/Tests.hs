-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module Tests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [ Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "1"        (SrcString "1")   (Eval [] (Value 1))
  , Test "div 0"        (SrcString "1 div 0")   (Eval [] RuntimeError)
  , Test "mod 0"        (SrcString "1 mod 0")   (Eval [] RuntimeError)
  , Test "lazy 1"        (SrcString "if true then 42 else 1 div 0")   (Eval [] (Value 42))
  , Test "type 1"        (SrcString "if true then 42 else true")   TypeError
  , Test "type 2"        (SrcString "if (if true then false else true) then 0 else 1") (Eval [] (Value 1))

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
  , Test "letbool1" (SrcString "let x = (if 1<2 then 1<2 else 1<2) in if x then 1 else 1")     (Eval [] (Value 1))
  , Test "letbool1" (SrcString "let x = (if 1<2 then 1<2 else 1<2) in if x then 1<2 else 1<2") TypeError
  , Test "undefVar" (SrcString "x")                TypeError
  , Test "test1"      (SrcString "input x y z in x*y*z") (Eval [2,3,4] (Value 24))
  , Test "test2"      (SrcString "input x y in x div y") (Eval [6,3] (Value 2))
  , Test "test3"      (SrcString "input x y in x div y") (Eval [5,3] (Value 1))
  , Test "test4"      (SrcString "input x y in x - y") (Eval [5,3] (Value 2))
  , Test "test5"      (SrcString "input x y in x mod y") (Eval [5,3] (Value 2))
  , Test "test6"      (SrcString "input x y in -x + y") (Eval [10,5] (Value (-5)))
  , Test "test7"      (SrcString "input x y z in x*y + z") (Eval [-2,3,10] (Value 4))
  , Test "test8"      (SrcString "input x y z in (x div y) + (x div z)") (Eval [10,5, 2] (Value 7))
  , Test "test9"      (SrcString "input x y z in (x * y) - (x div z)") (Eval [7, 5, 3] (Value 33))
  , Test "test10"     (SrcString "input x y z v in ((x mod y) mod z) mod v") (Eval [100,27, 10, 5] (Value 4))

  , Test "test110"     (SrcString "input x y in if x<y then x else y") (Eval [1,2] (Value 1))
  , Test "test11"     (SrcString "input x y in if true then x else y") (Eval [1,2] (Value 1))




  , Test "test12"     (SrcString "input x y in if false then x else y") (Eval [1,2] (Value 2))
  , Test "test13"     (SrcString "input x y in if not false then x else y") (Eval [1,2] (Value 1))
  , Test "test14"     (SrcString "input x y in if not true then x else y") (Eval [1,2] (Value 2))
  , Test "test15"     (SrcString "input x in let t = 5 in x + t") (Eval [5] (Value 10))
  , Test "test16"     (SrcString "input x y z  in let t = (if x < y then 1 else 0) in z + t") (Eval [0, 1, 5] (Value 6))
  , Test "test17"     (SrcString "input x y z  in let t = (if x < y then 1 else 0) in z + t") (Eval [1, 0, 5] (Value 5))
  , Test "test18"     (SrcString "input x in x + true") TypeError
  , Test "test19"     (SrcString "input x in if 5 then false else x") TypeError
  , Test "test20"     (SrcString "input x in x + y") TypeError
  , Test "test21"     (SrcString "input x in if true then false else x") TypeError
  , Test "test22"     (SrcString "input x in let t = (if x = 1 then true else false) in not t") TypeError
  , Test "test23"     (SrcString "input x in if x = 5 then true else false") TypeError
  , Test "test24"     (SrcString "input x y in let t = x * y in x or y or t") TypeError
  , Test "test25"     (SrcString "input x in true or false") TypeError
  , Test "test26"     (SrcString "input x in if x mod 2 = 1 then true else false") TypeError
  , Test "test27"     (SrcString "input x in if x * 3 = 14 then x+3 or x else 5") TypeError
  , Test "test28"     (SrcString "input x in not true") TypeError
  , Test "test29"     (SrcString "input x in if not false then not (x + 3) else x") TypeError
  , Test "test30"     (SrcString "input x in if true then true or false else true and false") TypeError


  , Test "test_plik1_1" (SrcFile "Tests/test_plik1.pp4") (Eval [1,2, 3, 4] (Value 4))
  , Test "test_plik1_2" (SrcFile "Tests/test_plik1.pp4") (Eval [1,1, 3, 4] (Value 3))
  , Test "test_plik2_1" (SrcFile "Tests/test_plik2.pp4") (Eval [1,2, 3, 4] (Value 3))
  , Test "test_plik2_2" (SrcFile "Tests/test_plik2.pp4") (Eval [1,1, 3, 4] (Value 4))
  , Test "test_plik3_1" (SrcFile "Tests/test_plik3.pp4") (Eval [1,2, 3, 4] (Value 3))
  , Test "test_plik3_2" (SrcFile "Tests/test_plik3.pp4") (Eval [1,1, 3, 4] (Value 4))
  , Test "test_plik3_3" (SrcFile "Tests/test_plik3.pp4") (Eval [1,0, 3, 4] (Value 4))
  , Test "test_plik4_1" (SrcFile "Tests/test_plik4.pp4") (Eval [1,2, 3, 4] (Value 4))
  , Test "test_plik4_2" (SrcFile "Tests/test_plik4.pp4") (Eval [1,1, 3, 4] (Value 4))
  , Test "test_plik4_3" (SrcFile "Tests/test_plik4.pp4") (Eval [1,0, 3, 4] (Value 3))
  , Test "test_plik5_1" (SrcFile "Tests/test_plik5.pp4") (Eval [1,2, 3, 4] (Value 3))
  , Test "test_plik5_2" (SrcFile "Tests/test_plik5.pp4") (Eval [1,1, 3, 4] (Value 3))
  , Test "test_plik5_3" (SrcFile "Tests/test_plik5.pp4") (Eval [1,0, 3, 4] (Value 4))
  , Test "test_plik6_1" (SrcFile "Tests/test_plik6.pp4") (Eval [1,2, 3, 4] (Value 4))
  , Test "test_plik6_2" (SrcFile "Tests/test_plik6.pp4") (Eval [1,1, 3, 4] (Value 3))
  , Test "test_plik6_3" (SrcFile "Tests/test_plik6.pp4") (Eval [1,0, 3, 4] (Value 3))

  , Test "test_plik7_1" (SrcFile "Tests/test_plik7.pp4") (Eval [1,2, 3, 4] (Value 3))
  , Test "test_plik7_2" (SrcFile "Tests/test_plik7.pp4") (Eval [1,1, 3, 4] (Value 4))
  , Test "test_plik7_3" (SrcFile "Tests/test_plik7.pp4") (Eval [1,0, 3, 4] (Value 4))
  , Test "test_plik7_4" (SrcFile "Tests/test_plik7.pp4") (Eval [0,1, 5, 4] (Value 4))

  , Test "test_plik8_1" (SrcFile "Tests/test_plik8.pp4") (Eval [1,2, 3, 4] (Value 1))
  , Test "test_plik8_2" (SrcFile "Tests/test_plik8.pp4") (Eval [1,1, 3, 4] (Value 0))
  , Test "test_plik8_3" (SrcFile "Tests/test_plik8.pp4") (Eval [1,1, 3, 3] (Value 1))
  , Test "test_plik8_4" (SrcFile "Tests/test_plik8.pp4") (Eval [1,2, 4, 4] (Value 1))
  , Test "test_plik8_5" (SrcFile "Tests/test_plik8.pp4") (Eval [2,1, 5, 4] (Value 0))

  , Test "test_plik9_1" (SrcFile "Tests/test_plik9.pp4") (Eval [1,2, 3, 4] (Value 4))
  , Test "test_plik9_2" (SrcFile "Tests/test_plik9.pp4") (Eval [5,2, 3, 4] (Value 5))
  , Test "test_plik9_3" (SrcFile "Tests/test_plik9.pp4") (Eval [1,2, 5, 4] (Value 5))
  , Test "test_plik9_4" (SrcFile "Tests/test_plik9.pp4") (Eval [10,2, 30, 4] (Value 6))

  ]
