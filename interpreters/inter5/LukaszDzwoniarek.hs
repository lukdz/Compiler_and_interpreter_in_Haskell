{-
ghc Prac4.hs
./Prac4 -t

ghc Prac4.hs && ./Prac4 -t
-}

{-
Lista zmian wzgledem zadnia na Prac4 oddanego przez 274404:
-zmieniono na poprawną kolejność Left - Right
-dodano sprawdzanie typu zwracanego przez funkcję
-zmieniono obsługę typów z "int" na TInt
-mam sprawdzanie typów dla list, unit, match i funkcji
-przerobiono eval / ev na wewnętrzny tryb danych
-implementacje pary (chyba problemy z pętleniem się (ev ?)zostły usunięte)
-dodanie wypisywanie typow błędów (show t)
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

-- Funkcja sprawdzająca typy
-- Dla wywołania typecheck vars e zakładamy, że zmienne występujące
-- w vars są już zdefiniowane i mają typ int, i oczekujemy by wyrażenia e
-- miało typ int

typecheck :: [FunctionDef p] -> [Var] -> Expr p -> TypeCheckResult p

typecheck funcDef xs expr =
 case (t_prog,t_func) of
  (_, Error typ text) -> Error typ text
  (Right TInt, _) -> Ok
  (Right typ, _) -> Error (getData expr) ("Prog returns " ++ show typ)
  (Left (p, text), _) -> Error p text
 where
  t_prog = infer_type funcDef (gtCreate xs) expr
  t_func = func_type funcDef funcDef

func_type ::
 [FunctionDef p] -> [FunctionDef p] -> TypeCheckResult p
func_type func funcDef=
  case func of
    [] -> Ok
    x:xs -> case infer_type funcDef [(funcArg x, funcArgType x)] (funcBody x) of
            Left (p, text) -> Error p text
            Right tx -> if tx == funcResType x
                        then func_type xs funcDef
                        else Error (funcPos x)
                             ("Func type decered "  ++ show (funcResType x) ++
                             ", type recived " ++ show tx)

infer_type ::
 [FunctionDef p] -> [(Var,Type)] -> Expr p -> Either (p, [Char]) Type

-- stala / liczba
infer_type funcDef _ (ENum p a) = Right TInt
-- zmienna
infer_type funcDef gem (EVar p a) = case gtLookup a gem of
 Just a -> Right a
 Nothing -> Left (p, "Niezdefiniowana zmienna " ++ show a)
-- stała / bool
infer_type funcDef _ (EBool p w) = Right TBool
-- Neg Int
infer_type funcDef gem (EUnary p UNeg e1) =
 case t1 of
  Left error -> Left error
  Right TInt -> Right TInt
  Right TBool -> Left (p, "UNeg difrent type " ++ show e1)
 where
  t1 = infer_type funcDef gem e1

-- Not Bool
infer_type funcDef gem (EUnary p UNot e1) =
 case t1 of
  Left error -> Left error
  Right TBool -> Right TBool
  Right TInt -> Left (p, "UNot difrent type " ++ show e1)
 where
  t1 = infer_type funcDef gem e1

-- operator binarny
infer_type funcDef gem (EBinary p op e1 e2)
 | mathOperator op = math_type funcDef gem (EBinary p op e1 e2)
 | compOperator op = comp_type funcDef gem (EBinary p op e1 e2)
 | boolOperator op = bool_type funcDef gem (EBinary p op e1 e2)

-- let... in...
infer_type funcDef gem (ELet p var e1 e2) =
 let
  t1 = infer_type funcDef gem e1
 in
  if isLeft t1
  then t1
  else case t1 of
   Right t1 -> let gem1 = gtExtend (var,t1) gem in infer_type funcDef gem1 e2

-- if... then... else...
infer_type funcDef gem (EIf p e0 e1 e2) =
 case (t0, t1, t2) of
  (Left error, _, _) -> Left error   --trzy podobne definicje
  (_, Left error, _) -> Left error   --aby za kazdym razem
  (_, _, Left error) -> Left error   --przekazac informacje o bledzie
  (Right TBool, Right tt1, Right tt2) -> if tt1==tt2
                                         then t1
                                         else Left (p, "EIf difrent types " ++
                                                     show e1 ++" " ++ show e2)
  (_,_,_) -> Left (p, "EIf unknown error")
 where
  t0 = infer_type funcDef gem e0
  t1 = infer_type funcDef gem e1
  t2 = infer_type funcDef gem e2

--PRAC 5 START

-- Lista pusta (anotowana typem)
infer_type _ _ (ENil p ltyp) =
  case ltyp of
    TList _ -> Right ltyp
    _ -> Left (p, "list nil type: " ++
                   show ltyp)

-- Konstruktor listy niepustej
infer_type funcDef gem (ECons p exprX exprXS) =
 case (t_elem, t_list) of
  (Right t1, Right (TList t2)) -> if t1==t2
                          then Right (TList t2)
                          else Left (p, "list types not match x: " ++
                                         show t1 ++ ", xs: " ++ show (TList t2))
  (Left err, _) -> Left err
  (_, Left err) -> Left err
  _ -> Left (p, "list with non list type")
 where
  t_elem = infer_type funcDef gem exprX
  t_list = infer_type funcDef gem exprXS


-- Dopasowanie wzorca dla listy
infer_type funcDef gem (EMatchL p expr nil cons) =
 case in_type of
  Right (TList x) ->
   case (nil_type, cons_type) of
    (Right t1, Right t2) -> if t1==t2
                            then Right t1
                            else Left (p, "match return types not match " ++
                                           show t1 ++ ", xs: " ++ show t2)
    (Left err, _) -> Left err
    (_, Left err) -> Left err
   where
    nil_type = infer_type funcDef gem nil
    cons_type = consClause_type funcDef gem x cons
  Right t -> Left (p, "match arg is not list type " ++ show t)
  Left err -> Left err
 where
  in_type = infer_type funcDef gem expr


-- Aplikacja funkcji
infer_type funcDef gem (EApp p f_id arg ) =
 case (arg_def, arg_type) of
  (Nothing,_) -> Left (p, "FuncDef not found " ++ show f_id)
  (_, Left err) -> Left err
  (Just x, Right y) ->
            if (funcArgType x) == y
            then Right (funcResType x)
            else Left (p, "Func " ++ show f_id ++
                          " aplied arg: " ++ show y ++
                          " not match declaration" ++ show (funcArgType x))
 where
   arg_def = funcLookup f_id funcDef
   arg_type = infer_type funcDef gem arg
-- Wyrażenie ()
infer_type funcDef _ (EUnit p) = Right TUnit


-- Konstruktor pary
infer_type funcDef gem (EPair p l r) =
  case (tl, tp) of
    (Right ttl, Right ttp) -> Right (TPair ttl ttp)
    (Left err, _) -> Left err
    (_, Left err) -> Left err
  where
    tl = infer_type funcDef gem l
    tp = infer_type funcDef gem r

-- Pierwsza projekcja pary
infer_type funcDef gem (EFst p expr) =
  case infer_type funcDef gem expr of
    Right (TPair t _) -> Right t
    Right t -> Left (p, "EFst from not pair " ++ show t)
    Left err -> Left err

-- Druga projekcja pary
infer_type funcDef gem (ESnd p expr) =
  case infer_type funcDef gem expr of
    Right (TPair _ t) -> Right t
    Right t -> Left (p, "ESnd from not pair " ++ show t)
    Left err -> Left err

--odczytujemy typ funkcji ze slownika
funcLookup :: FSym -> [FunctionDef p] -> Maybe (FunctionDef p)
funcLookup y xs =
 case xs of
  [] -> Nothing
  (x:xss) -> if y == (funcName x)
           then Just x
           else funcLookup y xss

--obliczanie wartości typu zwracanego dla match dla listy niepuste
consClause_type :: [FunctionDef p]
     -> [(Var, Type)]
     -> Type
     -> (Var, Var, Expr p)
     -> Either (p, [Char]) Type
consClause_type funcDef gem typ (x, xs, expr) =
 infer_type funcDef gem2 expr
 where
  gem1 = gtExtend (x, typ) gem
  gem2 = gtExtend (xs, TList typ) gem1

--PRAC 5 END

math_type :: [FunctionDef t]
     -> [(Var, Type)] -> Expr t -> Either (t, [Char]) Type
math_type funcDef gem (EBinary p _ e1 e2) =
 case (t1,t2) of
  (Left error, _) -> Left error
  (_, Left error) -> Left error
  (Right TInt, Right TInt) -> Right TInt
  (Right t1, Right t2) -> Left (p, "EBinary math difrent types "
                                   ++ show t1 ++ " " ++ show t2)
 where
  t1 = infer_type funcDef gem e1
  t2 = infer_type funcDef gem e2

comp_type :: [FunctionDef t]
       -> [(Var, Type)] -> Expr t -> Either (t, [Char]) Type
comp_type funcDef gem (EBinary p _ e1 e2) =
 case (t1, t2) of
  (Left error, _) -> Left error
  (_, Left error) -> Left error
  (Right TInt, Right TInt) -> Right TBool
  (Right t1, Right t2) -> Left (p, "EBinary comp difrent types"
                                    ++ show t1 ++ " " ++ show t2)
 where
  t1 = infer_type funcDef gem e1
  t2 = infer_type funcDef gem e2

bool_type :: [FunctionDef t]
       -> [(Var, Type)] -> Expr t -> Either (t, [Char]) Type
bool_type funcDef gem (EBinary p _ e1 e2) =
 case (t1, t2) of
  (Left error, _) -> Left error
  (_, Left error) -> Left error
  (Right TBool, Right TBool) -> Right TBool
  (Right t1, Right t2) -> Left (p, "EBinary bool difrent types"
                                  ++ show t1 ++ " " ++ show t2)
 where
  t1 = infer_type funcDef gem e1
  t2 = infer_type funcDef gem e2

mathOperator :: BinaryOperator -> Bool
mathOperator op
 | op == BAdd = True
 | op == BSub = True
 | op == BMul = True
 | op == BDiv = True
 | op == BMod = True
 | otherwise = False

compOperator :: BinaryOperator -> Bool
compOperator op
 | op == BEq = True
 | op == BNeq= True
 | op == BLt = True
 | op == BGt = True
 | op == BLe = True
 | op == BGe = True
 | otherwise = False

boolOperator :: BinaryOperator -> Bool
boolOperator op
 | op == BAnd= True
 | op == BOr = True
 | otherwise = False

--tworzymy slownik
gtCreate :: [t] -> [(t, Type)]
gtCreate xs = map ( \x -> (x, TInt) ) xs

--odczytujemy ze slownika
gtLookup :: (Eq a, Eq a1) => a1 -> [(a1, a)] -> Maybe a
gtLookup y xs
 | xs == [] = Nothing
 | y == (fst.head $ xs) = Just (snd.head $ xs)
 | otherwise = gtLookup y (tail xs)

--rozszerza lub aktualizauje slownik
gtExtend :: (Eq a, Eq b) => (a, b) -> [(a, b)] -> [(a, b)]
gtExtend y xs
 | [] == xs = [y]
 | fst y == (fst.head $ xs) = (y:tail xs)
 | otherwise = head xs : gtExtend y (tail xs)















-- Funkcja obliczająca wyrażenia
-- Dla wywołania ev input e przyjmujemy, że dla każdej pary (x, v)
-- znajdującej się w input, wartość zmiennej x wynosi v.
-- Możemy założyć, że wyrażenie e jest dobrze typowane, tzn.
-- typecheck (map fst input) e = Ok

--tworzymy slownik
evGam :: [(Var,Integer)] -> [(Var,Val)]
evGam gamma =
 case gamma of
  [] -> []
  (a,b):xs -> (a, VNum b) : evGam xs
--odczytujemy ze slownika
env_lookup :: Eq a => a -> [(a,Val)] -> Maybe Val
env_lookup y xs
 | xs == [] = Nothing
 | y == (fst.head $ xs) = Just (snd.head $ xs)
 | otherwise = env_lookup y (tail xs)
--rozszerza lub aktualizauje slownik
env_extend :: Eq a => (a,Val) -> [(a,Val)] -> [(a,Val)]
env_extend y xs
 | [] == xs = [y]
 | fst y == (fst.head $ xs) = (y:tail xs)
 | otherwise = head xs : env_extend y (tail xs)

eval :: [FunctionDef p] -> [(Var,Integer)] -> Expr p -> EvalResult
eval funcDef gamma expr =
 case ev funcDef gamma1 expr of
  Just (VNum n) -> Value n
  Nothing -> RuntimeError
 where
   gamma1 = evGam gamma

data Val = VNum Integer | VBool Bool | VList Val Val | VNil | VPair Val Val
           deriving Eq

--główna funkcja - oblicza wartość
ev :: [FunctionDef p] -> [(Var,Val)] -> Expr p -> Maybe Val
ev funcDef _ (ENum op a) = Just (VNum a)
ev funcDef gamma (EVar op a) = case env_lookup a gamma of
 Just n -> Just n
 Nothing -> Nothing

ev funcDef gem (EBool p w) =
 Just (VBool w)

--obliczanie wartości operacji binarnych
--NAJPIERW WYLICZY w1, w2, CZY DOPASUJE WG. "op"
ev funcDef gem (EBinary p op e1 e2) =
 case (op, w1, w2) of
  (BAdd, Just (VNum n1), Just (VNum n2)) -> Just . VNum $ n1 + n2
  (BSub, Just (VNum n1), Just (VNum n2)) -> Just . VNum $ n1 - n2
  (BMul, Just (VNum n1), Just (VNum n2)) -> Just . VNum $ n1 * n2
  (BDiv, Just (VNum n1), Just (VNum n2)) ->
    if (n2 == 0) then Nothing else Just . VNum $ n1 `div` n2
  (BMod, Just (VNum n1), Just (VNum n2)) ->
    if (n2 == 0) then Nothing else Just . VNum $ n1 `mod` n2
  (BAnd, Just (VBool a), Just (VBool b)) -> Just . VBool $ a && b
  (BOr , Just (VBool a), Just (VBool b)) -> Just . VBool $ a || b
  (BEq , Just (VNum n1), Just (VNum n2)) -> Just (VBool (n1 == n2) )
  (BNeq, Just (VNum n1), Just (VNum n2)) -> Just (VBool (n1 /= n2) )
  (BLt , Just (VNum n1), Just (VNum n2)) -> Just (VBool (n1 <  n2) )
  (BGt , Just (VNum n1), Just (VNum n2)) -> Just (VBool (n1 >  n2) )
  (BLe , Just (VNum n1), Just (VNum n2)) -> Just (VBool (n1 <= n2) )
  (BGe , Just (VNum n1), Just (VNum n2)) -> Just (VBool (n1 >= n2) )
  (_, _, _) -> Nothing
 where
  w1 = ev funcDef gem e1
  w2 = ev funcDef gem e2

-- let... in...
ev funcDef gem (ELet p var e1 e2) =
 case ev funcDef gem e1 of
  Nothing -> Nothing
  Just n -> let gem1 = env_extend (var, n) gem in
                       ev funcDef gem1 e2

-- Neg Int
ev funcDef gem (EUnary p UNeg e1) =
 case w1 of
  Nothing -> Nothing
  Just (VNum n) -> Just (VNum (-n))
 where
  w1 = ev funcDef gem e1

-- if... then... else...
ev funcDef gem (EIf p e0 e1 e2) =
 case (w0, w1, w2) of
  (Just (VBool False), _, Just n)-> Just n
  (Just (VBool True), Just n, _)-> Just n
  (_, _, _) -> Nothing
 where
  w0 = ev funcDef gem e0
  w1 = ev funcDef gem e1
  w2 = ev funcDef gem e2

-- Not Bool
ev funcDef gem (EUnary p UNot e1) =
 case w1 of
  Just (VBool True) -> Just (VBool False)
  Just (VBool False) -> Just (VBool True)
  Nothing -> Nothing
 where
  w1 = ev funcDef gem e1


--PRAC 5 START
-- Aplikacja funkcji
ev funcDef gem (EApp p f_id arg ) =
 case (func_def, arg_fal) of
  (Just x, Just y) -> ev funcDef gem1 (funcBody x)
                    where gem1 = [(funcArg x, y)]
              --where gem1 = env_extend (funcArg x, y) gem
              --czy nie powinno być tylko (funcArg x, y)
  _ -> Nothing
 where
  func_def = funcLookup f_id funcDef
  arg_fal = ev funcDef gem arg

-- Lista pusta (anotowana typem)
ev funcDef gem (ENil _ typ) =
  Just VNil

-- Konstruktor listy niepustej
ev funcDef gem (ECons p exprX exprXS) =
 case (x, xs) of
  (Just x, Just xs) -> Just (VList x xs)
  _ -> Nothing
 where
  x  = ev funcDef gem exprX
  xs = ev funcDef gem exprXS

-- Wyrażenie ()
ev funcDef gem (EUnit p) =
 Just (VNum 1)
-- Left (p, "EIf difrent types")

--match
ev funcDef gem (EMatchL p expr nil (n, ns, cons) ) =
  case ev funcDef gem expr of
    Just VNil -> ev funcDef gem nil
    Just (VList x xs) -> ev funcDef gem2 cons
                         where
                           gem1 = env_extend (n, x) gem
                           gem2 = env_extend (ns, xs) gem1
    _ -> Nothing

-- Konstruktor pary
ev funcDef gem (EPair p l r) =
  case (tl, tp) of
    (Just ttl, Just ttp) -> Just (VPair ttl ttp)
    _ -> Nothing
  where
    tl = ev funcDef gem l
    tp = ev funcDef gem r

-- Pierwsza projekcja pary
ev funcDef gem (EFst p expr) =
  case ev funcDef gem expr of
    Just (VPair t _) -> Just t
    _ -> Nothing

-- Druga projekcja pary
ev funcDef gem (ESnd p expr) =
  case ev funcDef gem expr of
    Just (VPair _ t) -> Just t
    _ -> Nothing
