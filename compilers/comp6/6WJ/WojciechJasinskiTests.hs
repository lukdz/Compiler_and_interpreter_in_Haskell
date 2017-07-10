{-# LANGUAGE Safe #-}

module WojciechJasinskiTests(tests) where

import DataTypes

--for shorter definition of simple tests
test :: String -> String -> Integer -> Test
test name program result =
  Test name (SrcString program) (Eval [] (Value result))

testfile :: String -> TestAnswer -> Test
testfile name answer = Test name (SrcFile (name ++ ".pp6")) answer

-- includes simpler tests from previous assignment
tests :: [Test]
tests =
  foldl
  (\ls (x, y) -> (compareTests x y) ++ ls)
  [ Test "num" (SrcString "1") (Eval [] (Value 1))
  , Test "boolean" (SrcString "true") TypeError
  , Test "let" (SrcString "let x = 3 in x") (Eval [] (Value 3))
  , Test "let-let" (SrcString "let x = 5 in let y = 1 in x - y") (Eval [] (Value 4))
  , Test "overlapping let" (SrcString "let x = 6 in let x = 5 in x") (Eval [] (Value 5))
  , Test "overlapping input-let" (SrcString "input x in let x = 2*x in x") (Eval [3] (Value 6))
  , Test "double unary minus #1" (SrcString "-(-7)") (Eval [] (Value 7))
  , Test "unary minus (with let)" (SrcString "let x = -8 in -x") (Eval [] (Value 8))
  , Test "bad unary minus" (SrcString "- true") TypeError
  , Test "simple if" (SrcString "if true then 10 else 2") (Eval [] (Value 10))
  , Test "simple if2" (SrcString "if false then 2 else 11") (Eval [] (Value 11))
  , Test "not simple if" (SrcString "if not true then 9 else 12") (Eval [] (Value 12))
  , Test "bad if #1" (SrcString "if 1 then 13 else 0") TypeError
  , Test "bad if #2" (SrcString "if true then 14 else false") TypeError
  , Test "bad if #3" (SrcString "if false then true else 15") TypeError
  , Test "kinda bad if" (SrcString "if false then true else false") TypeError
  , Test "nested if" (SrcString "if if false then false else true then 17 else 0") (Eval [] (Value 17))
  , Test "badly nested if" (SrcString "if if false then 1 else 0 then 18 else 0") TypeError
  , Test "true and" (SrcString "if true and true then 19 else 0") (Eval [] (Value 19))
  , Test "false and #1" (SrcString "if true and false then 0 else 20") (Eval [] (Value 20))
  , Test "false and #2" (SrcString "if false and true then 0 else 21") (Eval [] (Value 21))
  , Test "false and #3" (SrcString "if false and false then 0 else 22") (Eval [] (Value 22))
  , Test "false or" (SrcString "if false or false then 0 else 23") (Eval [] (Value 23))
  , test "true or #1" "if true or false then 24 else 0" 24
  , test "true or #2" "if false or true then 25 else 0" 25
  , test "true or #3" "if true or true then 26 else 0" 26
  , test "operators #1" "7+2*10" 27
  , test "operators #2" "-2 * -14" 28
  , test "operators #3" "5 * 4 div 2 mod 4 + 27" 29
  , test "operators #4" "20 div 2 + 3 * 10 - 50 mod 20" 30
  , Test "check both" (SrcString "if true then 31 else false") TypeError
  , test "execute one #1" "if true then 32 else 1 div 0" 32
  , test "operator precedence #5" "if true or true and false then 33 else 0" 33
  , test "operator precedence #6" "if not true or true then 34 else 0" 34
  , Test "another zero div" (SrcString "let x = 35 in let y = 0 in x div y") (Eval [] RuntimeError)
  , test "execute one #2" "if false then 1 div 0 else 36" 36
  , test "operator precedence #7" "if 7 < 5 or 5 < 7 then 37 else 0" 37
  , test "double unary minus #2" "- -38" 38
  , test "logic variables #1" "let x = true in if x then 39 else 0" 39
  , test "logic variables #2" "let x = true in let y = false in if x and y then 0 else 40" 40
  , Test "flag" (SrcString "input x in let f = x < 10 in if f then 5 else 41") (Eval [11] (Value 41))
  , Test "random stuff" (SrcFile "some1.pp6") (Eval [5, 20, 22] (Value 42))
  , Test "huh2" (SrcString "1 * (if 2 < 3 then 43 else 1)") (Eval [] (Value 43))
  -- new stuff begins here
  , testfile "fun" (Eval [] (Value 1))
  , test "pair1" "fst (2, 0)" 2
  , test "pair2" "snd (0, 3)" 3
  , testfile "pattern1"  (Eval [] (Value 4))
  , testfile "pattern2" (Eval [] (Value 5))
  , testfile "inthead1" (Eval [] RuntimeError)
  , testfile "inthead2" (Eval [] (Value 6))
  , testfile "pairfun" (Eval [] (Value 7))
  , testfile "simple map" (Eval [] (Value 8))
  , testfile "recursion1" (Eval [] (Value 9))
  , testfile "fibslow" (Eval [6] (Value 8))
  , testfile "fibfast" (Eval [32] (Value 2178309))
  , testfile "noglobals" TypeError
  , testfile "unitfun" (Eval [] (Value 13))
  , testfile "zip2int" (Eval [] (Value 14))
  --
  , testfile "loop" (Eval [2] RuntimeError)
  , Test "eager pair" (SrcString "fst (1, 1 div 0)") (Eval [] RuntimeError)
  , testfile "eager list" (Eval [] RuntimeError)
  , Test "eager list#2" (SrcString "fst (1, [1, 1 div 0] : int list)") (Eval [] RuntimeError)
  , Test "eager let" (SrcString "let x = 1 div 0 in 5") (Eval [] RuntimeError)
  , Test "bad list#1" (SrcString "let x = [1, true] : bool list in 1") TypeError
  , Test "bad list#2" (SrcString "let x = [1, true] : int list in 1") TypeError
  -- pracownia 6.
  , test "simple lambda" "(fn (a:int) -> a + 1) 2" 3
  , test "lambda lambda" "(fn (f : int -> int) -> f 1) (fn (x : int) -> 4 * x)" 4
  , testfile "closure" (Eval [] $ Value 5)
  , testfile "intmap" (Eval [] $ Value 6)
  , testfile "intfold" (Eval [] $ Value 7)
  , testfile "intfold#2" (Eval [] $ Value 8)
  , testfile "merge sort" (Eval [] $ Value 9)
  , testfile "non-rec let" TypeError
  ]
  -- the number pairs for comparison tests (argument to that foldl 30+ lines above)
  [(1, 3), (4, 4), (3, 5), (-3, 3), (0, 1), (5, 5), (9, 3), (5, 4), (4, 5)]

compareTests :: Integer -> Integer -> [Test]
compareTests a b =
  [ aux (<) a b "<"
  , aux (>) a b ">"
  , aux (<=) a b "<="
  , aux (>=) a b ">="
  , aux (==) a b "="
  , aux (/=) a b "<>"
  ]
  where aux comp a b compstring =
          let (trueResult, falseResult) = if comp a b then (1, 0) else (0, 1) in
            test
            ("comp " ++ (show a) ++ " " ++ compstring ++ " " ++ (show b))
            ("if " ++ (show a) ++ compstring ++ (show b) ++ " then "
              ++ (show trueResult) ++ " else " ++ (show falseResult))
            1
        
