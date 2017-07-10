{-# LANGUAGE Safe #-}

module JanSierpinaTests(tests) where

import DataTypes

tests :: [Test]
tests =
  [
  -- TYPE ERRORS --
  Test "inc" (SrcString "input x in x + 1") (Eval [42] (Value 43))
  , Test "undefVar" (SrcString "x") TypeError
  , Test "boolResult" (SrcString "true") TypeError
  , Test "if_types" (SrcString "if true then 3 else false") TypeError
  , Test "if_types_ok" (SrcString "if true then true else false") TypeError
  , Test "bin_type_error" (SrcString "if 3>true then 1 else 1") TypeError
  , Test "arith_type_error" (SrcString "7*true") TypeError
  , Test "let_type_error" (SrcString "let x=true in x+3") TypeError
  , Test "let_type_error_2" (SrcString "let x=true in if true then x else 0") TypeError
  , Test "boolProgram" (SrcString "snd (1, true)") TypeError
  , Test "multDifferentTypes" (SrcString "fst (1, 2) * snd (1, true)") TypeError
  , Test "listNotPair" (SrcString "snd ([1,2]: int list)") TypeError
  , Test "intNotPair" (SrcString "snd 1") TypeError
  , Test "ifTypeError" (SrcString "if true then fst (1,true) else snd (1,true)") TypeError
  , Test "emptyListAsResult" (SrcString "[] : int") TypeError
  , Test "listOfTwoTypes" (SrcString "let x=(1 :: true :: []: int list) in 2") TypeError
  , Test "matchNotList" (SrcString "let x=1 in match x with [] -> 1 | y::ys -> 2") TypeError
  , Test "matchTwoTypes" (SrcString "let y=(let x=[1]: int in match x with [] -> 1 | y::ys -> true) in 1") TypeError
  , Test "fun1TypeError" (SrcString "fun fun1 (x : int) : int = true in 1") TypeError
  , Test "fun2TypeError" (SrcString "fun fun2 (x : bool) : int = 1 in fun2(2)") TypeError
  , Test "fun3TypeError" (SrcString "fun fun3 (x : int) : bool = 1 in 1") TypeError
  , Test "fun4TypeError" (SrcString "fun fun4 (x : int) : int = true in fun4(false)") TypeError
  , Test "fun5TypeError" (SrcString "fun fun5 (x : int * bool) : int = snd x in fun5((1,false))") TypeError

  , Test "lambaMistype" (SrcString "fn(x:bool)->fn(y:int)->x+y") TypeError
  , Test "lambaWrngArg" (SrcString "let y=(1,1) in (fn(x:int)->x) y") TypeError
  , Test "lambaVarNotDef" (SrcString "(fn(x:int)-> x + snd y ) 3") TypeError
  , Test "lambaExtVar" (SrcString "let y=(1,true) in (fn(x:int)-> x + snd y ) 3") TypeError
  , Test "lambaWithLambda" (SrcString "(fn(f:int->int)-> f 1)(fn(f:bool)-> 1)") TypeError
  , Test "wrongApp" (SrcString "5 6") TypeError


  -- VALUES --
  , Test "let"      (SrcString "let v=3+1 in 3*v+1") (Eval [] (Value 13))
  , Test "int"      (SrcString "input x in x") (Eval [4] (Value 4))
  , Test "bin_op"   (SrcString "3*4+10") (Eval [] (Value 22))
  , Test "un_op"    (SrcString "-(1+2)") (Eval [] (Value (-3)))
  , Test "if"       (SrcString "if true then 3 div 0 else 3") (Eval [] RuntimeError)
  , Test "arith_ops"    (SrcString "-(1+2*7-(3 div 2))") (Eval [] (Value (-14)))
  , Test "mod_op"    (SrcString "(5*7) mod 6") (Eval [] (Value 5))
  , Test "mod2_op"    (SrcString "35 mod 6 mod 4") (Eval [] (Value 1))
  , Test "sum_3"    (SrcString "input a b c in a+b+c") (Eval [3, -5, 4] (Value 2))
  , Test "delta"    (SrcString "input a b c in b*b-4*a*c") (Eval [1, 2, 3] (Value (-8)))
  , Test "cube"    (SrcString "input x in x*x*x") (Eval [5] (Value 125))
  , Test "add42"    (SrcString "input x in let y=42 in x+y") (Eval [3] (Value 45))
  , Test "equal"    (SrcString "if 1=2 then 1 else 0") (Eval [] (Value 0))
  , Test "not_equal"    (SrcString "if 1+1<>2*2 then 1 else 0") (Eval [] (Value 1))
  , Test "less"    (SrcString "if 3-5<0 then 1 else 0") (Eval [] (Value 1))
  , Test "greater"    (SrcString "if 3+4>4+5 then 1 else 0") (Eval [] (Value 0))
  , Test "less_eq"    (SrcString "if 3<=3 then 1 else 0") (Eval [] (Value 1))
  , Test "greater_eq"    (SrcString "if 3-5>=0 then 1 else 0") (Eval [] (Value 0))
  , Test "or"    (SrcString "if true or false then 1 else 0") (Eval [] (Value 1))
  , Test "and"    (SrcString "if 1=1 and 2=3 then 1 else 0") (Eval [] (Value 0))
  , Test "and_eq"    (SrcString "input x in if x=x and x=x then 1 else 0") (Eval [3] (Value 1))
  , Test "not"    (SrcString "if not(4<=3 or true) then 1 else 0") (Eval [] (Value 0))
  , Test "zero_division"    (SrcString "input x in 3 div (2-x)") (Eval [2] RuntimeError)
  , Test "zero_ok"    (SrcString "if true then 3 else 4 div 0") (Eval [] (Value 3))
  , Test "big_number"    (SrcString "1000000*1000000*1000000*1000000*1000000") (Eval [] (Value 1000000000000000000000000000000))
  , Test "sqrt_file_10"    (SrcFile "sqrt.pp6") (Eval [10] (Value 3))
  , Test "sqrt_file_120"    (SrcFile "sqrt.pp6") (Eval [120] (Value 10))
  , Test "sqrt_file_121"    (SrcFile "sqrt.pp6") (Eval [121] (Value 11))
  , Test "fib_4"    (SrcFile "fib.pp6") (Eval [4] (Value 3))
  , Test "snd"    (SrcString "snd (1,2)") (Eval [] (Value 2))
  , Test "fst"    (SrcString "fst (1+1,2+2)") (Eval [] (Value 2))
  , Test "match"    (SrcString "let a=[3]:int list in match a with [] -> 1 | x::xs -> x") (Eval [] (Value 3))
  , Test "simple"    (SrcFile "simple.pp6") (Eval [] (Value 2))
  , Test "length"    (SrcFile "length.pp6") (Eval [] (Value 6))
  , Test "scalar"    (SrcFile "scalar.pp6") (Eval [] (Value 11))

  , Test "lambaWithExt" (SrcString "let x=7 in (fn(y:int)->x+y) 5") (Eval [] (Value 12))
  , Test "lambaDb" (SrcString "(fn(y:int)->fn(x:int)->x*y) 5 6") (Eval [] (Value 30))
  , Test "map"    (SrcFile "map.pp6") (Eval [] (Value 30))
  , Test "map2"    (SrcFile "map2.pp6") (Eval [7] (Value 105))
  , Test "flipminus" (SrcFile "flipminus.pp6") (Eval [3, 5] (Value 2))
  , Test "unMinus" (SrcString "let unMin = fn(y:int)->(-y) in unMin 5") (Eval [] (Value (-5)))
  , Test "double_succ" (SrcString "let succ = fn(x:int)->(x+1) in let dfn = fn(fx:(int->int)*int)-> fst fx (fst fx (snd fx)) in dfn (succ, 4)") (Eval [] (Value 6))
  ]
