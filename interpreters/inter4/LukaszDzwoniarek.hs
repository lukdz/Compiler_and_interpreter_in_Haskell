{-
ghc Prac4.hs
./Prac4 -t

ghc Prac4.hs && ./Prac4 -t
-}


-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko} gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module LukaszDzwoniarek (typecheck, eval) where

-- Importujemy moduły z definicją języka oraz typami potrzebnymi w zadaniu
import AST
import DataTypes
import Data.Either
--import Data.List
--import Data.Either.Unwrap

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int

-- LukaszDzwoniarek.typecheck [] (ENum "pos2" 1)
-- LukaszDzwoniarek.typecheck [("a")] (EVar "pos2" "a")
typecheck :: [Var] -> Expr p -> TypeCheckResult p

typecheck xs expr =
 case t1 of
  Left "int" -> Ok
  Left "bool" -> Error (getData expr) "Prog returns bool"
  Left _ -> Error (getData expr) "Unknown type error"
  Right (p, text) -> Error p text
 where
  t1 = infer_type (gtCreate xs) expr

data TType = TInt | TBool

infer_type :: [(Var,[Char])] -> Expr p -> Either [Char] (p, [Char])

-- stala / liczba
infer_type _ (ENum p a) = Left "int"
-- zmienna
infer_type gem (EVar p a) = case gtLookup a gem of
 Just a -> Left a
 Nothing -> Right (p, "Niezdefiniowana zmienna")
-- stała / bool
infer_type _ (EBool p w) = Left "bool"

-- Neg Int
infer_type gem (EUnary p UNeg e1) =
 case t1 of
  Right error -> Right error
  Left "int" -> Left "int"
  Left "bool" -> Right (p, "UNeg difrent type")
 where
  t1 = infer_type gem e1

-- Not Bool
infer_type gem (EUnary p UNot e1) =
 case t1 of
  Right error -> Right error
  Left "bool" -> Left "bool"
  Left "int" -> Right (p, "UNot difrent type")
 where
  t1 = infer_type gem e1

-- operator binarny
infer_type gem (EBinary p op e1 e2)
 | mathOperator op = math_type gem (EBinary p op e1 e2)
 | compOperator op = comp_type gem (EBinary p op e1 e2)
 | boolOperator op = bool_type gem (EBinary p op e1 e2)

-- let... in...
infer_type gem (ELet p var e1 e2) =
 let
  t1 = infer_type gem e1
 in
  if isRight t1
  then t1
  else case t1 of
   Left t1 -> let gem1 = gtExtend (var,t1) gem in infer_type gem1 e2

-- if... then... else...
infer_type gem (EIf p e0 e1 e2) =
 case (t0, t1, t2) of
  (Right error, _, _) -> Right error   --trzy podobne definicje
  (_, Right error, _) -> Right error   --aby za kazdym razem
  (_, _, Right error) -> Right error   --przekazac informacje o bledzie
  (Left "bool", Left "int", Left "int") -> Left "int"
-- CZY TO JEST POPRAWNE ?:
  (Left "bool", Left "bool", Left "bool") -> Left "bool"
  (Left _, Left _, Left _) -> Right (p, "EIf difrent types")
 where
  t0 = infer_type gem e0
  t1 = infer_type gem e1
  t2 = infer_type gem e2

-- LukaszDzwoniarek.infer_type [] (EBinary "pos1" BAdd (ENum "pos2" 1) (ENum "pos3" 1))
math_type gem (EBinary p _ e1 e2) =
 case (t1,t2) of
  (Right error, _) -> Right error
  (_, Right error) -> Right error
  (Left "int", Left "int") -> Left "int"
  (Left _, Left _) -> Right (p, "EBinary math difrent types")
 where
  t1 = infer_type gem e1
  t2 = infer_type gem e2

-- LukaszDzwoniarek.infer_type [] (EBinary "pos1" BNeq (ENum "pos2" 1) (ENum "pos3" 1))
comp_type gem (EBinary p _ e1 e2) =
 case (t1, t2) of
  (Right error, _) -> Right error
  (_, Right error) -> Right error
  (Left "int", Left "int") -> Left "bool"
  (Left _, Left _) -> Right (p, "EBinary comp difrent types")
 where
  t1 = infer_type gem e1
  t2 = infer_type gem e2

-- poprawne:
-- LukaszDzwoniarek.infer_type [] (EBinary "pos1" BAnd (EBinary "pos1" BNeq (ENum "pos2" 1) (ENum "pos3" 1)) (EBinary "pos1" BNeq (ENum "pos2" 1) (ENum "pos3" 1)))
-- NIE poprawne:
-- LukaszDzwoniarek.infer_type [] (EBinary "pos1" BAnd (ENum "pos2" 1) (ENum "pos3" 1))
bool_type gem (EBinary p _ e1 e2) =
 case (t1, t2) of
  (Right error, _) -> Right error
  (_, Right error) -> Right error
  (Left "bool", Left "bool") -> Left "bool"
  (Left _, Left _) -> Right (p, "EBinary bool difrent types")
 where
  t1 = infer_type gem e1
  t2 = infer_type gem e2

mathOperator op
 | op == BAdd = True
 | op == BSub = True
 | op == BMul = True
 | op == BDiv = True
 | op == BMod = True
 | otherwise = False

compOperator op
 | op == BEq = True
 | op == BNeq= True
 | op == BLt = True
 | op == BGt = True
 | op == BLe = True
 | op == BGe = True
 | otherwise = False

boolOperator op
 | op == BAnd= True
 | op == BOr = True
 | otherwise = False

--tworzymy slownik
gtCreate xs = map varInt xs
varInt x = (x, "int")
--odczytujemy ze slownika
gtLookup y xs
 | xs == [] = Nothing
 | y == (fst.head $ xs) = Just (snd.head $ xs)
 | otherwise = gtLookup y (tail xs)
--rozszerza lub aktualizauje slownik
gtExtend y xs
 | [] == xs = [y]
 | fst y == (fst.head $ xs) = (y:tail xs)
 | otherwise = head xs : gtExtend y (tail xs)









-- Funkcja obliczająca wyrażenia
-- Dla wywołania eval input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok

--odczytujemy ze slownika
--DLACZEGO POTRZEBA "Eq b" -> do porównania z [], ale przecież jest pusta
env_lookup :: Eq a => Eq b => a -> [(a,b)] -> Maybe b
env_lookup y xs
 | xs == [] = Nothing
 | y == (fst.head $ xs) = Just (snd.head $ xs)
 | otherwise = env_lookup y (tail xs)
--rozszerza lub aktualizauje slownik
env_extend :: Eq a => Eq b => (a,b) -> [(a,b)] -> [(a,b)]
env_extend y xs
 | [] == xs = [y]
 | fst y == (fst.head $ xs) = (y:tail xs)
 | otherwise = head xs : env_extend y (tail xs)

--główna funkcja - oblicza wartość
eval :: [(Var,Integer)] -> Expr p -> EvalResult
eval _ (ENum op a) = Value (a)
eval gamma (EVar op a) = case env_lookup a gamma of
 Just n -> Value n
 Nothing -> RuntimeError

--True = Value 1
--False = Value 0
eval gem (EBool p w) =
 case w of
  True -> Value 1
  False -> Value 0

--obliczanie wartości operacji binarnych
--NAJPIERW WYLICZY w1, w2, CZY DOPASUJE WG. "op"
eval gem (EBinary p op e1 e2) =
 case (op, w1, w2) of
  (BAdd, Value n1, Value n2) -> Value (n1 + n2)
  (BSub, Value n1, Value n2) -> Value (n1 - n2)
  (BMul, Value n1, Value n2) -> Value (n1 * n2)
  (BDiv, Value n1, Value n2) ->
    if (n2 == 0) then RuntimeError else Value (n1 `div` n2)
  (BMod, Value n1, Value n2) ->
    if (n2 == 0) then RuntimeError else Value (n1 `mod` n2)
  (BAnd, Value 1, Value 1) -> Value 1
  (BAnd, Value 1, Value 0) -> Value 0
  (BAnd, Value 0, Value 1) -> Value 0
  (BAnd, Value 0, Value 0) -> Value 0
  (BOr, Value 1, Value 1) -> Value 1
  (BOr, Value 1, Value 0) -> Value 1
  (BOr, Value 0, Value 1) -> Value 1
  (BOr, Value 0, Value 0) -> Value 0
  (BEq , Value n1 , Value n2) ->
    if (n1 == n2) then Value 1 else Value 0
  (BNeq, Value n1 , Value n2) ->
    if (n1 /= n2) then Value 1 else Value 0
  (BLt , Value n1 , Value n2) ->
    if (n1 <  n2) then Value 1 else Value 0
  (BGt , Value n1 , Value n2) ->
    if (n1 >  n2) then Value 1 else Value 0
  (BLe , Value n1 , Value n2) ->
    if (n1 <= n2) then Value 1 else Value 0
  (BGe , Value n1 , Value n2) ->
    if (n1 >= n2) then Value 1 else Value 0
  (_, _, _) -> RuntimeError
 where
  w1 = eval gem e1
  w2 = eval gem e2

-- let... in...
eval gem (ELet p var e1 e2) =
 case eval gem e1 of
  RuntimeError -> RuntimeError
  Value n -> let gem1 = env_extend (var, n) gem in eval gem1 e2

-- Neg Int
eval gem (EUnary p UNeg e1) =
 case w1 of
  RuntimeError -> RuntimeError
  Value n -> Value (- n)
 where
  w1 = eval gem e1

-- if... then... else...
eval gem (EIf p e0 e1 e2) =
 case (w0, w1, w2) of
  (Value 0, _, Value n)-> Value n
  (Value 1, Value n, _)-> Value n
  (_, _, _) -> RuntimeError
 where
  w0 = eval gem e0
  w1 = eval gem e1
  w2 = eval gem e2

-- Not Bool
eval gem (EUnary p UNot e1) =
 case w1 of
  Value 1 -> Value 0
  Value 0 -> Value 1
  RuntimeError -> RuntimeError
 where
  w1 = eval gem e1
