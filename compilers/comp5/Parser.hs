-- Parser języka PP4
module Parser (parseProgram) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token

import AST

setPos :: SourcePos -> Expr SourcePos -> Expr SourcePos
setPos = setData

lang :: LanguageDef st
lang = emptyDef
  { commentStart    = "(*"
  , commentEnd      = "*)"
  , nestedComments  = True
  , reservedNames   =
    [ "and", "bool", "div", "elif", "else", "false", "fst", "fun", "if", "in"
    , "input", "int", "let", "list", "match", "mod", "not", "or", "snd", "then"
    , "true", "unit", "with"
    ]
  , reservedOpNames =
    [ "|", ",", ":", "->", "::"
    , "+", "-", "*", "=", "<>", "<", ">", "<=", ">=" ]
  }

tokenParser :: TokenParser st
tokenParser = makeTokenParser lang

ws = whiteSpace tokenParser

keyword :: String -> Parser ()
keyword = reserved tokenParser

kwAnd   = keyword "and"
kwBool  = keyword "bool"
kwDiv   = keyword "div"
kwElif  = keyword "elif"
kwElse  = keyword "else"
kwFalse = keyword "false"
kwFst   = keyword "fst"
kwFun   = keyword "fun"
kwIf    = keyword "if"
kwIn    = keyword "in"
kwInput = keyword "input"
kwInt   = keyword "int"
kwLet   = keyword "let"
kwList  = keyword "list"
kwMatch = keyword "match"
kwMod   = keyword "mod"
kwNot   = keyword "not"
kwOr    = keyword "or"
kwSnd   = keyword "snd"
kwThen  = keyword "then"
kwTrue  = keyword "true"
kwUnit  = keyword "unit"
kwWith  = keyword "with"

op :: String -> Parser ()
op = reservedOp tokenParser

opBar   = op "|"
opComma = op ","
opColon = op ":"
opArrow = op "->"
opCons  = op "::"

opEq  = op "="
opNeq = op "<>"
opLt  = op "<"
opGt  = op ">"
opLe  = op "<="
opGe  = op ">="

opPlus  = op "+"
opMinus = op "-"
opAster = op "*"

ident :: Parser String
ident = identifier tokenParser <?> "identifier"

type Parser = Parsec String ()

program :: Parser (Program SourcePos)
program = ws *>
  (uncurry Program <$> programPreamble <*> expr <* eof)

programPreamble :: Parser ([FunctionDef SourcePos], [(Var,SourcePos)])
programPreamble = choice
  [ (,)    <$> many1 functionDef <*> inputVarsIn
  , (,) [] <$> (kwInput *> many1 preambleVar <* kwIn)
  , return ([], [])
  ] <?> "program preamble"
  where
    inputVarsIn = choice
      [ (kwInput *> many1 preambleVar <* kwIn)
      , (kwIn *> return [])
      ]

preambleVar :: Parser (Var,SourcePos)
preambleVar =
  (\ pos x -> (x,pos)) <$> getPosition <*> ident

programFunctions :: Parser [FunctionDef SourcePos]
programFunctions = choice
  [ many1 functionDef <* kwIn
  , return []
  ] <?> "function definitions"

functionDef :: Parser (FunctionDef SourcePos)
functionDef =
  mkFunctionDef
  <$> (getPosition <* kwFun)
  <*> ident
  <*> parens tokenParser
        ((,) <$> ident <*> (opColon *> typeExpr))
  <*> (opColon *> typeExpr)
  <*> (opEq    *> expr)
  where
    mkFunctionDef p f (x, t1) t2 e =
      FunctionDef p f x t1 t2 e

typeExpr :: Parser Type
typeExpr =
  (buildPairType <$> appTypeExpr `sepBy1` opAster) <?> "type"
  where
    buildPairType = foldr1 TPair
    buildListType = foldl (\ t _ -> TList t)

    appTypeExpr = buildListType <$> simpleTypeExpr <*> many kwList

    simpleTypeExpr = choice
      [ kwInt  *> return TInt
      , kwBool *> return TBool
      , kwUnit *> return TUnit
      , parens tokenParser typeExpr
      ]

expr :: Parser (Expr SourcePos)
expr = choice
  [ letExpr
  , ifExpr
  , matchExpr
  , opExpr
  ] <?> "expression"

letExpr :: Parser (Expr SourcePos)
letExpr =
  ELet <$> getPosition <*>
  (kwLet *> ident) <*> (opEq *> expr) <*> (kwIn *> expr)

ifExpr :: Parser (Expr SourcePos)
ifExpr =
  EIf <$> getPosition <*> (kwIf *> expr) <*> (kwThen *> expr) <*> ifExprCont
  where
    ifExprCont = choice
      [ kwElse *> expr
      , EIf <$> getPosition <*>
        (kwElif *> expr) <*> (kwThen *> expr) <*> ifExprCont
      ]

matchExpr :: Parser (Expr SourcePos)
matchExpr =
  EMatchL <$> getPosition <*>
    (kwMatch *> expr <* kwWith) <*> nilClause <*> consClause
  where
    nilClause =
      optional opBar *>
      brackets tokenParser (return ()) *>
      opArrow *>
      expr
    consClause =
      (\ x y e -> (x, y, e))
      <$> (opBar *> ident)
      <*> (opCons *> ident)
      <*> (opArrow *> expr)

opExpr :: Parser (Expr SourcePos)
opExpr =
  opLeftAssoc  opOr             $
  opLeftAssoc  opAnd            $
  opUnary      opNot            $
  opNoAssoc    opCmp            $
  consExpr                      $
  opLeftAssoc  opAdditive       $
  opLeftAssoc  opMultiplicative $
  opUnary      opUMinus         $
  appExpr
  where
    opOr  = kwOr  *> return BOr
    opAnd = kwAnd *> return BAnd
    opNot = kwNot *> return UNot
    opCmp = choice
      [ opEq  *> return BEq
      , opNeq *> return BNeq
      , opLt  *> return BLt
      , opGt  *> return BGt
      , opLe  *> return BLe
      , opGe  *> return BGe
      ]
    opAdditive = choice
      [ opPlus  *> return BAdd
      , opMinus *> return BSub
      ]
    opMultiplicative = choice
      [ opAster *> return BMul
      , kwDiv   *> return BDiv
      , kwMod   *> return BMod
      ]
    opUMinus = opMinus *> return UNeg

consExpr :: Parser (Expr SourcePos) -> Parser (Expr SourcePos)
consExpr p = parser where
  parser = buildCons <$> p <*> consTail
  consTail = choice
    [ Just <$> (opCons *> parser)
    , return Nothing
    ]
  buildCons e1 Nothing   = e1
  buildCons e1 (Just e2) = ECons (getData e1) e1 e2

appExpr :: Parser (Expr SourcePos)
appExpr = choice
  [ varExpr <$> getPosition <*> ident <*> (optionMaybe simpleExpr)
  , EFst <$> getPosition <*> (kwFst *> simpleExpr)
  , ESnd <$> getPosition <*> (kwSnd *> simpleExpr)
  , simpleExpr
  ]
  where
    varExpr pos x Nothing  = EVar pos x
    varExpr pos f (Just e) = EApp pos f e

simpleExpr :: Parser (Expr SourcePos)
simpleExpr = choice
  [ setPos <$> getPosition <*> parens tokenParser parenExpr
  , listExpr
  , ENum   <$> getPosition <*> natural tokenParser
  , EBool  <$> getPosition <*> boolConst
  , EVar   <$> getPosition <*> ident
  ]

parenExpr :: Parser (Expr SourcePos)
parenExpr =
  buildParenExpr <$> expr `sepBy` opComma <*> getPosition
  where
    buildParenExpr [] pos = EUnit pos
    buildParenExpr es _   = foldr1 buildPair es
    
    buildPair e1 e2 = EPair (getData e1) e1 e2

listExpr :: Parser (Expr SourcePos)
listExpr =
  mkList
  <$> getPosition
  <*> brackets tokenParser ((,) <$> expr `sepBy` opComma <*> getPosition)
  <*> (opColon *> typeExpr)
  where
    mkList p1 (es, p2) tp =
      setPos p1 (foldr buildList (ENil p2 tp) es)
    buildList e1 e2 = ECons (getData e1) e1 e2

boolConst = choice
  [ kwTrue  *> return True
  , kwFalse *> return False
  ]

-- Helper functions

opLeftAssoc op exp =
  buildExpr <$> getPosition <*> exp <*> cont
  where
    cont = choice
      [ contExpr <$> op <*> exp <*> cont
      , return (const id)
      ]
    contExpr o e2 f pos e1 = f pos (EBinary pos o e1 e2)

opNoAssoc op exp =
  buildExpr <$> getPosition <*> exp <*> cont
  where
    cont = choice
      [ buildBinary <$> op <*> exp
      , return (const id)
      ]
    buildBinary o e2 pos e1 = EBinary pos o e1 e2

buildExpr pos e1 f = f pos e1

opUnary op exp = p
  where
    p :: Parser (Expr SourcePos)
    p = choice
      [ EUnary <$> getPosition <*> op <*> p
      , exp
      ]
    
-- Main function
parseProgram :: SourceName -> String -> Either ParseError (Program SourcePos)
parseProgram = parse program
