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
-- UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jej definicje
compile :: [FunctionDef p] -> [Var] -> Expr p -> [MInstr]
--compile = undefined
compile funcDef var expr =
    case runComp program labelNew of
      (_, instr) -> instr
    where
      (labelNew, stack) = stackCreate funcDef var (LabelDef 0)
      (funStart, funEnd) = ([],MPopN 0) --funcStack (stackFunc stack) --funcGamma
      program = funStart
                |@ comp stack exprVar
                @| [funEnd, MRet]
                --lambdy maina
                -- @@ (\lable -> (lable with lambdas lable = [] , lambdas lable) )
                @@ compFunc funcDef stack --funcGamma
                --lambdy funkcji
                -- @@ (\lable -> (lable with lambdas lable = [] , lambdas lable) )
      exprVar = freeVars expr
-------------------------------------------------------------------------------

freeVars :: Expr p -> Expr [Var]
freeVars (EVar p var) =
  EVar [var] var

freeVars (ENum p n) =
  ENum [] n

freeVars (EBool p b) =
  EBool [] b

freeVars (EUnary p unaryOperator e) =
    EUnary xs unaryOperator f
  where
    f = freeVars e
    xs = AST.getData f

freeVars (EBinary p binaryOperator e1 e2) =
    EBinary xs binaryOperator f1 f2
  where
    f1 = freeVars e1
    f2 = freeVars e2
    xs = union (AST.getData f1) (AST.getData f2)

freeVars (ELet p var e1 e2) =
    ELet xs var f1 f2
  where
    f1 = freeVars e1
    f2 = freeVars e2
    xs = union (AST.getData f1) . delete var $ (AST.getData f2)

freeVars (EIf p e0 e1 e2) =
    EIf xs f0 f1 f2
  where
    f0 = freeVars e0
    f1 = freeVars e1
    f2 = freeVars e2
    xs = union (AST.getData f0) . union (AST.getData f1) $ AST.getData f2

freeVars (EFn p var typ e) =
    EFn xs var typ f
  where
    f = freeVars e
    xs = delete var (AST.getData f)

freeVars (EApp p eFunc eArg) =
    EApp xs fFunc fArg
  where
    fFunc = freeVars eFunc
    fArg = freeVars eArg
    xs = union (AST.getData fArg) (AST.getData fFunc)

freeVars (EUnit p) =
  EUnit []

freeVars (EPair p e1 e2) =
    EPair xs f1 f2
  where
    f1 = freeVars e1
    f2 = freeVars e2
    xs = union (AST.getData f1) (AST.getData f2)

freeVars (EFst p e) =
    EFst xs f
  where
    f = freeVars e
    xs = AST.getData f

freeVars (ESnd p e) =
    ESnd xs f
  where
    f = freeVars e
    xs = AST.getData f

freeVars (ENil p typ) =
  ENil [] typ

freeVars (ECons p e1 e2) =
    ECons xs f1 f2
  where
    f1 = freeVars e1
    f2 = freeVars e2
    xs = union (AST.getData f1) (AST.getData f2)

freeVars (EMatchL p e0 e1 (var1, var2, e2)) =
    EMatchL xs f0 f1 (var1, var2, f2)
  where
    f0 = freeVars e0
    f1 = freeVars e1
    f2 = freeVars e2
    xs = union (AST.getData f0) . union (AST.getData f1)
         . delete var1 . delete var2 $ AST.getData f2
-------------------------------------------------------------------------------
-- STOS START
--height i gamma są potrzebne, ponieważ MGetLocal przyjmuje adresy od góry stosu
data StackDef = StackDef
  { stackHeight      :: Int             --aktualna wysokość stosu
  , stackGamma       :: [(Var,Int)]     --słownik zmienna -> pozycja na stosie od dołu stosu
  , stackFunc        :: [(Var,Label)]  --słownik funkcja -> etytkieta
  }
  deriving (Eq)

stackCreate :: [FunctionDef p] -> [Var] -> LabelDef -> (LabelDef, StackDef)
stackCreate funcDef var label =
    (labelNew, StackDef height gamma func)
  where
    height   = length gamma -- + length func
    gamma    = unfoldr stackVar var
    --gamma    = unfoldr stackVar (map funcName funcDef ++ var)
    (func, labelNew) = stackFun funcDef label

stackCreateF :: Var -> [(Var,Label)] -> StackDef
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

stackFun :: [FunctionDef p] -> LabelDef -> ( [(Var,Label)], LabelDef )
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

gammaLookup :: Var -> [(Var,Int)] -> Maybe Int
gammaLookup var gamma =
 case gamma of
  --[] -> 666 --tylko do debugu (w kodzie wynikowym łatwo dostrzec taką wartość)
  [] -> Nothing --tylko do debugu (w kodzie wynikowym łatwo dostrzec taką wartość)
  x:xs -> if var == fst x
          then Just $ snd x
          else gammaLookup var xs

funcLookup :: Var -> [(Var,Label)] -> Label
funcLookup var gamma =
 case gamma of
  [] -> 555 --tylko do debugu (w kodzie wynikowym łatwo dostrzec taką wartość)
  x:xs -> if var == fst x
          then snd x
          else funcLookup var xs


funcStack :: [(Var,Label)] -> ([MInstr], MInstr)
funcStack stackFunc =
  case stackFunc  of
    []   -> ([], MPopN 0)
    (_, label):xs -> let (instr, _) = funcStack xs in
                      ( instr
                        ++ [MAlloc 1, MPush, MGetLabel label, MSet 0]
                      , MPopN $ length stackFunc)
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
  --lambdas    :: [MInstr]
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
comp :: StackDef -> Expr [Var] -> Comp


comp _ (ENum p n) =
  addInstr [MConst n]


comp stack (EVar p var) =
  case gammaLookup var (stackGamma stack) of
    Just pos -> let n = stackHeight stack - pos in
                addInstr [MGetLocal n]
    Nothing -> let label = funcLookup var (stackFunc stack) in
                addInstr [MAlloc 1, MPush, MGetLabel label, MSet 0, MPopAcc]


comp _ (EBool p n) =
  case n of
    False -> addInstr [MConst mFalse]
    True  -> addInstr [MConst mTrue]


comp stack (EBinary p op e1 e2) =
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
  w1 = comp stack e1
  w2 = comp (stackChange 1 stack) e2
  wT = addInstr [MConst mTrue]
  wF = addInstr [MConst mFalse]


comp stack (EUnary p op e) =
 case op of
  UNeg -> w @| [MNeg]
  UNot -> w @| [MNot]
 where
  w = comp stack e


comp stack (EIf p e eT eF) =
    w @@ addIf MC_NZ wT wF
  where
    w = comp stack e
    wT = comp stack eT
    wF = comp stack eF


comp stack (ELet p var e1 e2) =
  w1 @| [MPush] @@ w2 @| [MPopN 1]
 where
  w1 = comp stack e1
  w2 = comp (stackExtend var stack) e2
-------------------------------------------------------------------------------

--PAIR
comp stack (EPair p e1 e2) =
    --[MAlloc 2, MPush] |@ w1 @| [MSet 0] @@ w2 @| [MSet 1, MPopAcc]
    addRecord [w1, w2]
  where
    w1 = comp (stackChange 1 stack) e1
    w2 = comp (stackChange 1 stack) e2

comp stack (EFst p e) =
    w @| [MGet 0]
  where
    w = comp stack e

comp stack (ESnd p e) =
    w @| [MGet 1]
  where
    w = comp stack e
-------------------------------------------------------------------------------

--LIST
--MAlloc zwraca liczby parzyste z zakresu do 0 + długość programu do 8000 hex
--program ma długość >= 1 (musi zawierać przynajmniej instrukcję MRet)
--dlatego wartość mFalse / 0 można traktować jako []
comp stack (ENil p t) =
    addInstr [MConst mFalse]

--dla listy x:xs umieszczamy:
--w rekordzie 0 -> x
--w rekordzie 1 -> xs
comp stack (ECons p e1 e2) =
    --[MAlloc 2, MPush] |@ w1 @| [MSet 0] @@ w2 @| [MSet 1, MPopAcc]
    addRecord [w1, w2]
  where
    w1 = comp (stackChange 1 stack) e1
    w2 = comp (stackChange 1 stack) e2


comp stack (EMatchL p e en (x, xs, ec)) =
    w @@ addIf MC_Z wn wc
  where
    w  = comp stack e
    wn = comp stack en
    wc = [MPush, MGet 0, MPush, MGetLocal 1, MGet 1, MSetLocal 1]
         |@ comp (stackExtend x . stackExtend xs $ stack) ec
         @| [MPopN 2]
-------------------------------------------------------------------------------

--Aplikacja funkcji
comp stack (EApp p eFun eArg) =
    wArg @| [MPush] @@ wFun @| [MPush, MGet 0, MCallAcc, MPopN 2]
  where
    wArg = comp stack eArg
    wFun = comp (stackChange 1 stack) eFun


comp stack (EUnit p) =
    addInstr []
    --addInstr [MConst 777] --tylko do debugu (w kodzie wynikowym łatwo dostrzec taką wartość)
-------------------------------------------------------------------------------
--Lambda
comp stack (EFn p arg _ eFun) =
  Comp(\label ->
    let (labelNew, [lStart, lEnd]) = labelTwo label in
      runComp ( [MJump lEnd, MLabel lStart]
                |@ loadRecord (length p) @@ wFun
                    -- @| [MRet, MLabel lEnd, MAlloc n, MPush, MGetLabel lStart, MSet 0, MPopAcc]
                    @| [MPopN $ length p] -- usuwanie wczytanych zmiennych wolnych ze stosu
                    @| [MRet, MLabel lEnd, MAlloc n, MPush, MGetLabel lStart, MSet 0]
                    @@ saveRecord ( map (\v -> comp (stackChange 1 stack) v ) . map (\v -> EVar [] v ) $ p )
                    -- @@ saveRecord ( map (\_ -> comp (stackChange 1 stack) (ENum [] 444) ) $ p )
              )
              labelNew
      )
  where
    wFun = comp stackF eFun

    --stackF = stackChange (1+n) $ stackExtend arg stack
          -- foldr (+) 5 [1,2,3,4]
    stackF0 = stackChange 1 $ stackExtend arg stack
    stackF = foldr stackExtend stackF0 p

    n = 1 + length p
-------------------------------------------------------------------------------

addIf :: MCondition -> Comp -> Comp -> Comp
addIf cond wT wF =
  Comp(\label ->
    let (labelNew, [lTrue, lEnd]) = labelTwo label in
      runComp ( [MBranch cond lTrue]
                |@ wF @| [MJump lEnd, MLabel lTrue]
                @@ wT @| [MLabel lEnd]
              )
              labelNew
      )


addRecord :: [Comp] -> Comp
addRecord ws =
    [MAlloc n, MPush]
    |@ addR ws 0
    @| [MPopAcc]
  where
    n = length ws
    --addR wewnętrzna funkcja, która nie jest widoczna dla innych od addRecord
    addR :: [Comp] -> Int -> Comp
    addR w n =
      case w of
        [] -> addInstr []
        x:xs -> x @| [MSet n] @@ addR xs (n+1)


saveRecord :: [Comp] -> Comp
saveRecord ws =
    saveR ws 1
    @| [MPopAcc]
  where
    n = length ws
    --saveR wewnętrzna funkcja, która nie jest widoczna dla innych od addRecord
    saveR :: [Comp] -> Int -> Comp
    saveR w n =
      case w of
        [] -> addInstr []
        x:xs -> x @| [MSet n] @@ saveR xs (n+1)


loadRecord :: Int -> Comp
loadRecord n =
    loadR 0 n
  where
    --loadR wewnętrzna funkcja, która nie jest widoczna dla innych od addRecord
    loadR :: Int -> Int -> Comp
    loadR w n =
        if n > 0
          then [MGetLocal w, MGet n, MPush] |@ loadR (w+1) (n-1)
          else addInstr []

-------------------------------------------------------------------------------

compFunc :: [FunctionDef p] -> StackDef -> Comp
compFunc funcDef stack =
  case funcDef of
    []   -> addInstr []
    x:xs ->   [MLabel fId]
              |@ comp stackF (freeVars . funcBody $ x)
              @| [MRet]
              @@ compFunc xs stack
            where
              fId = funcLookup (funcName x) (stackFunc stack)
              stackF = stackChange 1 $ stackExtend (funcArg x) stack
