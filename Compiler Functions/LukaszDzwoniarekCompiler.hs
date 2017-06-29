{-
Debug pro-tips:
  - funcke mają lable od 0 do n,
  - lable w main są od n do m
  - lable w funkcjach są od m do końca

Historia zmian:
 - obsługa liczb i operacji matematycnych
 - obsługa boola i operacji porównania oraz boolowskich
 - obsługa zmiennych (StackDef)
 - obsługa LabelDef (poprawne generowani etykiet skoków)
 - obsługa par
 - obsługa list
 - zmieniono operator @@@ na @| i dodano operator |@
 - zmieniono operator @@| na @| i |@@ na |@
 - implementacja funkcji (wg. Prac5) zakończona i zdebugowana
 - zoptymalizowano pamięciowo działanie list
    (dla nila z 1+3 jednostek pamięci do 1, dla x:xs z 1+3 do 1+2)
 - przerzucono tworzeni stosu i czyszczenie stosu
    z wywołania funkcji do definicji funkcji
-}
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Compiler gdzie
-- za {Imie} i {Nazwisko} należy podstawić odpowiednio swoje imię
-- i nazwisko zaczynające się wielką literą oraz bez znaków diakrytycznych.
module LukaszDzwoniarekCompiler(compile) where

import AST
import MacroAsm
import Data.List


-- Funkcja kompilująca program
-- Dla pracowni nr 4 należy zignorować pierwszy argument
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicje
compile :: [FunctionDef p] -> [Var] -> Expr p -> [MInstr]
compile funcDef var expr =
 case runComp ( comp funcDef stack expr
                  @| [MRet]
                  @@ compFunc funcDef stack )
              labelNew of
   (_, instr) -> instr
 where
   (labelNew, stack) = stackCreate funcDef var (LabelDef 0)
-------------------------------------------------------------------------------
-- STOS START
-- Definicja stosu
data StackDef = StackDef
  { stackHeight      :: Int             --aktualna wysokość stosu
  , stackGamma       :: [(Var,Int)]     --słownik zmienna->pozycja na stosie
  , stackFunc        :: [(FSym,Label)]  --słownik funkcja->etytkieta
  }
  deriving (Eq)

stackCreate :: [FunctionDef p] -> [Var] -> LabelDef -> (LabelDef, StackDef)
stackCreate funcDef var label =
    (labelNew, StackDef height gamma func)
  where
    height   = length gamma
    gamma    = unfoldr stackVar var
    (func, labelNew) = stackFun funcDef label

stackCreateF :: Var -> [(FSym,Label)] -> StackDef
stackCreateF var func =
    StackDef height gamma func
  where
    height   = length gamma
    gamma    = unfoldr stackVar [var]

stackVar :: [Var] -> Maybe ( (Var,Int) , [Var] )
stackVar var =
  case var of
    [] -> Nothing
    xss@(x:xs)  -> Just ( (x, length xss), xs )

stackFun :: [FunctionDef p] -> LabelDef -> ( [(FSym,Label)], LabelDef )
stackFun func label =
  case func of
    []   ->     ([], label)
    x:xs ->     ( y:ys , labelLast )
              where
                (labelNew, pos) = labelOne label
                y  = (funcName x, pos)
                (ys, labelLast) = stackFun xs labelNew

-- w słowniku mogą być dwa tłumaczenia etykiet na wartość
-- w ten sposób działa szybciej i dużo oszczędniej
-- jeśli chodzi o pamięć (trwałe struktury danych)
-- w porównaniu do nadpisywania w przypadku dwóch zmiennych o tej samej nazwie
stackExtend :: Var -> StackDef -> StackDef
stackExtend var stack =
    StackDef height gamma func
  where
    height = stackHeight stack + 1
    gamma  = (var,height) : stackGamma stack
    func   = stackFunc stack

stackChange :: Int -> StackDef -> StackDef
stackChange change stack =
    StackDef height gamma func
  where
    height = stackHeight stack + change
    gamma  = stackGamma stack
    func   = stackFunc stack

gammaLookup :: Var -> [(Var,Int)] -> Int
gammaLookup var gamma =
 case gamma of
  --[] -> 666 --FOR DEBUG ONLY
  x:xs -> if var == fst x
          then snd x
          else gammaLookup var xs
funcLookup :: FSym -> [(FSym,Label)] -> Label
funcLookup var gamma =
 case gamma of
  --[] -> 555 --FOR DEBUG ONLY
  x:xs -> if var == fst x
          then snd x
          else funcLookup var xs

-- STOS END
-------------------------------------------------------------------------------
-- COMP START
data LabelDef = LabelDef
  { current    :: Int        --aktualne id etykiety
  }
  deriving (Show, Eq)

newtype Comp = Comp
  { runComp    :: LabelDef -> (LabelDef, [MInstr])
  }

(@@) :: Comp -> Comp -> Comp
c1 @@ c2 =
  Comp (\ label0 -> let (label1, instr1) = runComp c1 label0 in
                    let (label2, instr2) = runComp c2 label1 in
                      (label2, instr1 ++ instr2)
       )

(@|) :: Comp -> [MInstr] -> Comp
c @| instr =
  c @@ (addInstr instr)

(|@) :: [MInstr] -> Comp -> Comp
instr |@ c =
  (addInstr instr) @@ c

-- gdzie można zamiast addInstr, stosować @| lub |@ -> krótszy zapis
addInstr :: [MInstr] -> Comp
addInstr instr =
  Comp (\ x -> (x, instr) )
-------------------------------------------------------------------------------
mTrue :: Integer
mTrue = -1 -- / 65535 / Neg 0 = 1111111111111111 bin
mFalse :: Integer
mFalse = 0
-------------------------------------------------------------------------------
labelOne :: LabelDef -> (LabelDef, Label)
labelOne x =
  ( LabelDef (current x + 1), current x)
labelTwo :: LabelDef -> (LabelDef, [Label])
labelTwo x0 =
  let (x1, l1) = labelOne x0 in
  let (x2, l2) = labelOne x1 in
  ( x2, [l1,l2] )
-------------------------------------------------------------------------------
comp :: [FunctionDef p] -> StackDef -> Expr p -> Comp


comp funcDef _ (ENum p n) =
  addInstr [MConst n]


comp funcDef stack (EVar p var) =
    addInstr [MGetLocal n]
  where
    n = stackHeight stack - gammaLookup var (stackGamma stack)


comp funcDef _ (EBool p n) =
  case n of
    False -> addInstr [MConst mFalse]
    True  -> addInstr [MConst mTrue]


comp funcDef stack (EBinary p op e1 e2) =
 case op of
  BAdd -> n @| [MAdd]
  BSub -> n @| [MSub]
  BMul -> n @| [MMul]
  BDiv -> n @| [MDiv]
  BMod -> n @| [MMod]
  BEq  -> n @@ addIf MC_EQ nT nF
  BNeq -> n @@ addIf MC_NE nT nF
  BLt  -> n @@ addIf MC_LT nT nF
  BGt  -> n @@ addIf MC_GT nT nF
  BLe  -> n @@ addIf MC_LE nT nF
  BGe  -> n @@ addIf MC_GE nT nF
  BAnd -> n @| [MAnd]
  BOr  -> n @| [MOr]
 where
  n1 = comp funcDef stack e1
  n2 = comp funcDef (stackChange 1 stack) e2
  n  = n1 @| [MPush] @@ n2
  nT = addInstr [MConst mTrue]
  nF = addInstr [MConst mFalse]


comp funcDef stack (EUnary p op e) =
 case op of
  UNeg -> n @| [MNeg]
  UNot -> n @| [MNot]
 where
  n = comp funcDef stack e


comp funcDef stack (EIf p e0 e1 e2) =
    w0 @@ addIf MC_NZ w1 w2
  where
    w0 = comp funcDef stack e0
    w1 = comp funcDef stack e1
    w2 = comp funcDef stack e2


comp funcDef stack (ELet p var e1 e2) =
  w1 @| [MPush] @@ w2 @| [MPopN 1]
 where
  w1 = comp funcDef stack e1
  w2 = comp funcDef (stackExtend var stack) e2

-------------------------------------------------------------------------------
--PAIR
comp funcDef stack (EPair p e1 e2) =
    [MAlloc 2, MPush] |@ w1 @| [MSet 0] @@ w2 @| [MSet 1, MPopAcc]
  where
    w1 = comp funcDef (stackChange 1 stack) e1
    w2 = comp funcDef (stackChange 1 stack) e2

comp funcDef stack (EFst p e) =
    w @| [MGet 0]
  where
    w = comp funcDef stack e

comp funcDef stack (ESnd p e) =
    w @| [MGet 1]
  where
    w = comp funcDef stack e

-------------------------------------------------------------------------------
--LIST
--MAlloc zwraca liczby parzyste z zakresu
--od długość programu (co najmniej dwa) do 8000hex
--dlatego wartość mFalse / 0 można traktować jako []
comp funcDef stack (ENil p t) =
    addInstr [MConst mFalse]

--dla listy x:xs umieszczamy:
--w rekordzie 0 -> x
--w rekordzie 1 -> xs
comp funcDef stack (ECons p e1 e2) =
    [MAlloc 2, MPush] |@ w1 @| [MSet 0] @@ w2 @| [MSet 1, MPopAcc]
  where
    w1 = comp funcDef (stackChange 1 stack) e1
    w2 = comp funcDef (stackChange 1 stack) e2


comp funcDef stack (EMatchL p e en (x, xs, ec)) =
    w @@ addIf MC_Z wn wc
  where
    w  = comp funcDef stack e
    wn = comp funcDef stack en
    wc = [MPush, MGet 0, MPush, MGetLocal 1, MGet 1, MSetLocal 1]
          |@ comp funcDef (stackExtend x . stackExtend xs $ stack) ec
          @| [MPopN 2]
-------------------------------------------------------------------------------
--Aplikacja funkcji
comp funcDef stack (EApp p fId e) =
    w @| [MCall fLabel]
  where
    w = comp funcDef stack e
    fLabel = funcLookup fId (stackFunc stack)


comp funcDef stack (EUnit p) =
    --addInstr []
    addInstr [MConst 777]--for debug only

-------------------------------------------------------------------------------
addIf :: MCondition -> Comp -> Comp -> Comp
addIf cond cT cF =
  Comp(\x ->
    let (xNew, [eFalse, eEnd]) = labelTwo x in
      runComp ( [MBranch cond eFalse]
                |@ cF @| [MJump eEnd, MLabel eFalse]
                @@ cT @| [MLabel eEnd]
              )
              xNew
      )

-------------------------------------------------------------------------------
compFunc :: [FunctionDef p] -> StackDef -> Comp

compFunc funcDef stack =
  case funcDef of
    [] -> addInstr []
    x:xs -> let stackF = stackCreateF (funcArg x) (stackFunc stack) in
                [MLabel fId, MPush]
                  |@ (comp funcDef stackF (funcBody x))
                  @| [MPopN 1, MRet]
                  @@ compFunc xs stack
            where fId = funcLookup (funcName x) (stackFunc stack)
