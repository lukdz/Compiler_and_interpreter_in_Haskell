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
    (dla nila z 1+3 jednostek pamięci do 1+0, dla x:xs z 1+3 do 1+2)
 - przerzucono tworzeni stosu i czyszczenie stosu
    z wywołania funkcji do definicji funkcji
-}
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający rozwiązanie.
module LukaszDzwoniarekCompiler(compile) where

import AST
import MacroAsm
import Data.List

-- Funkcja kompilująca program
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
--height i gamma są potrzebne, ponieważ MGetLocal przyjmuje adresy od góry stosu
data StackDef = StackDef
  { stackHeight      :: Int             --aktualna wysokość stosu
  , stackGamma       :: [(Var,Int)]     --słownik zmienna -> pozycja na stosie od dołu stosu
  , stackFunc        :: [(FSym,Label)]  --słownik funkcja -> etytkieta
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
stackFun funcDef label =
  case funcDef of
    []   ->     ([], label)
    x:xs ->     ( y:ys , labelLast )
              where
                (labelNew, pos) = labelOne label
                y  = (funcName x, pos)
                (ys, labelLast) = stackFun xs labelNew

-- jeśli mamy dwie zmienne o tej samej nazwie w słowniku występują dwa
-- tłumaczenia etykiet na wartość w ten sposób działa szybciej
-- i oszczędniej (pamięć w Haskellu to trwałe struktury danych)
-- w porównaniu do nadpisywania w przypadku dwóch zmiennych o tej samej nazwie
stackExtend :: Var -> StackDef -> StackDef
stackExtend var stack =
    StackDef height gamma func
  where
    height = 1 + stackHeight stack
    gamma  = (var,height) : stackGamma stack
    func   = stackFunc stack

stackChange :: Int -> StackDef -> StackDef
stackChange delta stack =
    StackDef height gamma func
  where
    height = stackHeight stack + delta
    gamma  = stackGamma stack
    func   = stackFunc stack

gammaLookup :: Var -> [(Var,Int)] -> Int
gammaLookup var gamma =
 case gamma of
  --[] -> 666 --tylko do debugu (w kodzie wynikowym łatwo dostrzec taką wartość)
  x:xs -> if var == fst x
          then snd x
          else gammaLookup var xs

funcLookup :: FSym -> [(FSym,Label)] -> Label
funcLookup var gamma =
 case gamma of
  --[] -> 555 --tylko do debugu (w kodzie wynikowym łatwo dostrzec taką wartość)
  x:xs -> if var == fst x
          then snd x
          else funcLookup var xs
-- STOS END
-------------------------------------------------------------------------------
-- COMP START
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

-- stosowanie @| i |@ skraca i poprawia czytelność kodu
addInstr :: [MInstr] -> Comp
addInstr instr =
  Comp (\ x -> (x, instr) )
-- COMP END
-------------------------------------------------------------------------------
-- LabelDef START
data LabelDef = LabelDef
  { fresh    :: Int        --aktualne id etykiety
  }
  deriving (Show, Eq)

labelOne :: LabelDef -> (LabelDef, Label)
labelOne ld =
  ( LabelDef $ fresh ld + 1, fresh ld)

labelTwo :: LabelDef -> (LabelDef, [Label])
labelTwo ld =
  let (ld1, l1) = labelOne ld in
  let (ld2, l2) = labelOne ld1 in
  ( ld2, [l1,l2] )
-- LabelDef END
-------------------------------------------------------------------------------
-- CONST
mTrue :: Integer
mTrue = -1 -- = 65535 = Neg 0 = 1111111111111111 bin

mFalse :: Integer
mFalse = 0

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
  BAdd -> w @| [MAdd]
  BSub -> w @| [MSub]
  BMul -> w @| [MMul]
  BDiv -> w @| [MDiv]
  BMod -> w @| [MMod]
  BEq  -> w @@ addIf MC_EQ wT wF
  BNeq -> w @@ addIf MC_NE wT wF
  BLt  -> w @@ addIf MC_LT wT wF
  BGt  -> w @@ addIf MC_GT wT wF
  BLe  -> w @@ addIf MC_LE wT wF
  BGe  -> w @@ addIf MC_GE wT wF
  BAnd -> w @| [MAnd]
  BOr  -> w @| [MOr]
 where
  w  = w1 @| [MPush] @@ w2
  w1 = comp funcDef stack e1
  w2 = comp funcDef (stackChange 1 stack) e2
  wT = addInstr [MConst mTrue]
  wF = addInstr [MConst mFalse]


comp funcDef stack (EUnary p op e) =
 case op of
  UNeg -> w @| [MNeg]
  UNot -> w @| [MNot]
 where
  w = comp funcDef stack e


comp funcDef stack (EIf p e eT eF) =
    w @@ addIf MC_NZ wT wF
  where
    w = comp funcDef stack e
    wT = comp funcDef stack eT
    wF = comp funcDef stack eF


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
--MAlloc zwraca liczby parzyste z zakresu do 0 + długość programu do 8000 hex
--program ma długość >= 1 (musi zawierać przynajmniej instrukcję MRet)
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
    addInstr [MConst 777] --tylko do debugu (w kodzie wynikowym łatwo dostrzec taką wartość)
-------------------------------------------------------------------------------

addIf :: MCondition -> Comp -> Comp -> Comp
addIf cond wT wF =
  Comp(\label ->
    let (labelNew, [false, end]) = labelTwo label in
      runComp ( [MBranch cond false]
                |@ wF @| [MJump end, MLabel false]
                @@ wT @| [MLabel end]
              )
              labelNew
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
