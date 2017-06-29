-- Moduł z definicją języka PP4
{-# LANGUAGE Safe, DeriveFunctor #-}
module AST where

-- Zmienne
type Var  = String
type FSym = String

-- Typy
data Type
  = TInt
  | TBool
  | TUnit
  | TPair Type Type
  | TList Type
  deriving (Eq)

-- Operatory unarne
data UnaryOperator
  = UNot -- Operator boolowski not
  | UNeg -- Unarny minus
  deriving (Eq)

-- Operatory binarne
data BinaryOperator
  = BAnd | BOr            -- Operatory boolowskie: and or
  -- Operatory porównania
  | BEq | BNeq            -- Równość i różność
  | BLt | BGt | BLe | BGe -- Operatory < > <= >-
  -- Operatory arytmetyczne
  | BAdd | BSub           -- Addytywne: + -
  | BMul | BDiv | BMod    -- Multiplikatywne * div mod
  deriving (Eq)

-- Wyrażenia
data Expr p
  = EVar    p Var                              -- Zmienna
  | ENum    p Integer                          -- Literał całkowitoliczbowy
  | EBool   p Bool                             -- Stała boolowska
  | EUnary  p UnaryOperator (Expr p)           -- Wyrażenie operatorowe unarne
  | EBinary p BinaryOperator (Expr p) (Expr p) -- Wyrażenie operatorowe
  | ELet    p Var (Expr p) (Expr p)            -- Wyrażenie let
  | EIf     p (Expr p) (Expr p) (Expr p)       -- Wyrażenie warunkowe
  | EApp    p FSym (Expr p)                    -- Aplikacja funkcji
  | EUnit   p                                  -- Wyrażenie ()
  | EPair   p (Expr p) (Expr p)                -- Konstruktor pary
  | EFst    p (Expr p)                         -- Pierwsza projekcja pary
  | ESnd    p (Expr p)                         -- Druga projekcja pary
  | ENil    p Type                             -- Lista pusta (anotowana typem)
  | ECons   p (Expr p) (Expr p)                -- Konstruktor listy niepustej
  -- Dopasowanie wzorca dla listy
  | EMatchL p (Expr p) (NilClause p) (ConsClause p)
  deriving (Functor, Eq)

-- Klauzula dla listy pustej
type NilClause  p = Expr p

-- Klauzula dla listy niepustej
-- (x, y, e) oznacza "| x :: y -> e"
type ConsClause p = (Var, Var, Expr p)

-- Definicja funkcji
data FunctionDef p = FunctionDef
  { funcPos     :: p
  , funcName    :: FSym   -- Nazwa funkcji
  , funcArg     :: Var    -- Argument formalny
  , funcArgType :: Type   -- Typ argumentu
  , funcResType :: Type   -- Typ wyniku
  , funcBody    :: Expr p -- Ciało funkcji
  }
  deriving (Functor, Eq)

-- Kompletny program
data Program p = Program
  { functions :: [FunctionDef p]
  , inputVars :: [(Var,p)]
  , body      :: Expr p
  }
  deriving (Functor, Eq)

-- ============================================================================
-- Podstawowe operacje na wyrażeniach i programach

updateData :: (p -> p) -> Expr p -> Expr p
updateData f (EVar    p x)        = EVar    (f p) x
updateData f (ENum    p n)        = ENum    (f p) n
updateData f (EBool   p b)        = EBool   (f p) b
updateData f (EUnary  p op e)     = EUnary  (f p) op e
updateData f (EBinary p op e1 e2) = EBinary (f p) op e1 e2
updateData f (ELet    p x  e1 e2) = ELet    (f p) x  e1 e2
updateData f (EIf     p e1 e2 e3) = EIf     (f p) e1 e2 e3
updateData f (EApp    p ff e)     = EApp    (f p) ff e
updateData f (EUnit   p)          = EUnit   (f p)
updateData f (EPair   p e1 e2)    = EPair   (f p) e1 e2
updateData f (EFst    p  e)       = EFst    (f p) e
updateData f (ESnd    p  e)       = ESnd    (f p) e
updateData f (ENil    p  t)       = ENil    (f p) t
updateData f (ECons   p e1 e2)    = ECons   (f p) e1 e2
updateData f (EMatchL p e  c1 c2) = EMatchL (f p) e  c1 c2

setData :: p -> Expr p -> Expr p
setData p = updateData (\ _ -> p)

getData :: Expr p -> p
getData (EVar    p _)     = p
getData (ENum    p _)     = p
getData (EBool   p _)     = p
getData (EUnary  p _ _)   = p
getData (EBinary p _ _ _) = p
getData (ELet    p _ _ _) = p
getData (EIf     p _ _ _) = p
getData (EApp    p _ _)   = p
getData (EUnit   p)       = p
getData (EPair   p _ _)   = p
getData (EFst    p _)     = p
getData (ESnd    p _)     = p
getData (ENil    p _)     = p
getData (ECons   p _ _)   = p
getData (EMatchL p _ _ _) = p

instance Show Type where
  showsPrec _ TInt          = showString "int"
  showsPrec _ TBool         = showString "bool"
  showsPrec _ TUnit         = showString "unit"
  showsPrec p (TPair t1 t2) =
    showParen (p > 0) (showsPrec 1 t1 . showString " * " . showsPrec 0 t2)
  showsPrec _ (TList t)     = showsPrec 1 t . showString " list"

instance Show UnaryOperator where
  show UNot = "not"
  show UNeg = "-"

instance Show BinaryOperator where
  show BAnd = "and"
  show BOr  = "or"
  show BEq  = "="
  show BNeq = "<>"
  show BLt  = "<"
  show BGt  = ">"
  show BLe  = "<="
  show BGe  = ">="
  show BAdd = "+"
  show BSub = "-"
  show BMul = "*"
  show BDiv = "div"
  show BMod = "mod"

instance Show (Expr p) where
  showsPrec _ (EVar  _ x)     = showString x
  showsPrec _ (ENum  _ n)     = shows n
  showsPrec _ (EBool _ True)  = showString "true"
  showsPrec _ (EBool _ False) = showString "false"
  showsPrec p (EUnary _ op e) =
    showParen (p > p') (shows op . showChar ' ' . showsPrec p' e)
    where
      p' = unaryOperatorPrec op
      unaryOperatorPrec :: UnaryOperator -> Int
      unaryOperatorPrec UNot = 30
      unaryOperatorPrec UNeg = 70

  showsPrec p (EBinary _ op e1 e2) =
    showParen (p > p')
      (showsPrec leftPrec e1 . showChar ' ' . shows op . showChar ' '
      . showsPrec rightPrec e2)
    where
      (p',leftPrec,rightPrec) = binaryOperatorPrec op
      binaryOperatorPrec :: BinaryOperator -> (Int, Int, Int)
      binaryOperatorPrec BAnd = (20, 20, 21)
      binaryOperatorPrec BOr  = (10, 10, 11)
      binaryOperatorPrec BEq  = (40, 41, 41)
      binaryOperatorPrec BNeq = (40, 41, 41)
      binaryOperatorPrec BLt  = (40, 41, 41)
      binaryOperatorPrec BGt  = (40, 41, 41)
      binaryOperatorPrec BLe  = (40, 41, 41)
      binaryOperatorPrec BGe  = (40, 41, 41)
      binaryOperatorPrec BAdd = (50, 50, 51)
      binaryOperatorPrec BSub = (50, 50, 51)
      binaryOperatorPrec BMul = (60, 60, 61)
      binaryOperatorPrec BDiv = (60, 60, 61)
      binaryOperatorPrec BMod = (60, 60, 61)

  showsPrec p (ELet _ x e1 e2) =
    showParen (p > 0)
      (showString "let " . showString x . showString " = " . showsPrec 0 e1
      . showString " in " . showsPrec 0 e2)

  showsPrec p (EIf _ e1 e2 e3) =
    showParen (p > 0)
      (showString "if " . showsPrec 0 e1 . showString " then "
      . showsPrec 0 e2 . showElse e3)
    where
      showElse (EIf _ e1 e2 e3) =
        showString " elif " . showsPrec 0 e1 . showString " then "
        . showsPrec 0 e2 . showElse e3
      showElse e = showString " else " . showsPrec 0 e

  showsPrec p (EApp _ ff e) =
    showParen (p > 100)
      (showString ff . showString " " . showsPrec 101 e)

  showsPrec _ (EUnit _) = showString "()"
  
  showsPrec _ (EPair _ e1 e2) =
    showString "(" . showsPrec 0 e1 .
    showString ", " . showsPair e2 . showString ")"
    where
      showsPair (EPair _ e1 e2) =
        showsPrec 0 e1 . showString ", " . showsPair e2
      showsPair e = showsPrec 0 e

  showsPrec p (EFst _ e) =
    showParen (p > 100) (showString "fst " . showsPrec 101 e)

  showsPrec p (ESnd _ e) =
    showParen (p > 100) (showString "snd " . showsPrec 101 e)

  showsPrec _ (ENil _ tp) = showString "[] : " . shows tp

  showsPrec p (ECons _ e1 e2) =
    case showTail e2 of
      Left (tl, tp) ->
        showString "[" . showsPrec prec1 e1 . tl . showString "]" . shows tp
      Right tl ->
        showParen (p > prec) (showsPrec prec1 e1 . showString " : " . tl)
    where
      showTail (ENil _ tp)     = Left (showString "", tp)
      showTail (ECons _ e1 e2) =
        case showTail e2 of
          Left (tl, tp) -> Left(showString ", " . showsPrec prec1 e1 . tl, tp)
          Right tl -> Right$ showsPrec prec1 e1 . showString " :: " . tl
      showTail e = Right$ showsPrec prec e

      prec  = 45
      prec1 = prec + 1

  showsPrec p (EMatchL _ e e1 (x, xs, e2)) =
    showParen (p > 0) (
      showString "match " . showsPrec 0 e . showString " with" .
      showString " [] -> " . showsPrec 0 e1 .
      showString " | " . showString x . showString " :: " . showString xs .
      showString " -> " . showsPrec 0 e2)

instance Show (FunctionDef p) where
  showsPrec _ (FunctionDef _ f x t1 t2 e) =
    showString "fun " . showString f .
    showString "(" . showString x .
    showString " : " . shows t1 . showString ") : " . shows t2 .
    showString " = " . shows e

instance Show (Program p) where
  showsPrec _ (Program fdefs ivs e) =
    showFunctionDefs fdefs . showInputVars fdefs ivs . shows e
    where
      showFunctionDefs fdefs =
        flip (foldr (\ fd -> shows fd . showString "\n")) fdefs
      showInputVars [] [] = showString ""
      showInputVars _  [] = showString "in\n"
      showInputVars _  xs =
        showString "input "
        . flip (foldr (\ (x,_) -> showString x . showChar ' ')) xs
        . showString "in\n"
