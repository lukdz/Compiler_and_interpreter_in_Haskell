-- Moduł z definicją języka PP4
{-# LANGUAGE Safe #-}
module AST where

-- Zmienne
type Var = String

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
  deriving Eq

-- Kompletny program
data Program p = Program
  { inputVars :: [(Var,p)]
  , body      :: Expr p
  }
  deriving Eq

-- ============================================================================
-- Podstawowe operacje na wyrażeniach i programach

instance Functor Expr where
  fmap f (EVar    p x)        = EVar    (f p) x
  fmap f (ENum    p n)        = ENum    (f p) n
  fmap f (EBool   p b)        = EBool   (f p) b
  fmap f (EUnary  p op e)     = EUnary  (f p) op (fmap f e)
  fmap f (EBinary p op e1 e2) = EBinary (f p) op (fmap f e1) (fmap f e2)
  fmap f (ELet    p x  e1 e2) = ELet    (f p) x  (fmap f e1) (fmap f e2)
  fmap f (EIf     p e1 e2 e3) = EIf (f p) (fmap f e1) (fmap f e2) (fmap f e3)

updateData :: (p -> p) -> Expr p -> Expr p
updateData f (EVar    p x)        = EVar    (f p) x
updateData f (ENum    p n)        = ENum    (f p) n
updateData f (EBool   p b)        = EBool   (f p) b
updateData f (EUnary  p op e)     = EUnary  (f p) op e
updateData f (EBinary p op e1 e2) = EBinary (f p) op e1 e2
updateData f (ELet    p x  e1 e2) = ELet    (f p) x  e1 e2
updateData f (EIf     p e1 e2 e3) = EIf     (f p) e1 e2 e3

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

instance Show (Program p) where
  showsPrec _ (Program [] e) = shows e
  showsPrec _ (Program xs e) =
    showString "input "
    . flip (foldr (\ (x,_) -> showString x . showChar ' ')) xs
    . showString "in\n" . shows e
