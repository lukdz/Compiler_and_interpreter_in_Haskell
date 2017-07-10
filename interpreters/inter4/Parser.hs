-- Parser języka PP4
module Parser (parseExpr, parseProgram) where

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Token
import Control.Applicative ((<$>), (<*>), (<*), (*>))

import AST

setPos :: SourcePos -> Expr SourcePos -> Expr SourcePos
setPos = setData

lang :: LanguageDef st
lang = emptyDef
  { commentStart    = "(*"
  , commentEnd      = "*)"
  , nestedComments  = True
  , reservedNames   =
    [ "and", "div", "elif", "else", "false", "if", "in", "input", "let", "mod"
    , "not", "or", "then", "true"
    ]
  , reservedOpNames = [ "+", "-", "*", "=", "<>", "<", ">", "<=", ">=" ]
  }

tokenParser :: TokenParser st
tokenParser = makeTokenParser lang

ws = whiteSpace tokenParser

keyword :: String -> Parser ()
keyword = reserved tokenParser

kwAnd   = keyword "and"
kwDiv   = keyword "div"
kwElif  = keyword "elif"
kwElse  = keyword "else"
kwFalse = keyword "false"
kwIf    = keyword "if"
kwIn    = keyword "in"
kwInput = keyword "input"
kwLet   = keyword "let"
kwMod   = keyword "mod"
kwNot   = keyword "not"
kwOr    = keyword "or"
kwThen  = keyword "then"
kwTrue  = keyword "true"

op :: String -> Parser ()
op = reservedOp tokenParser

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
ident = identifier tokenParser

type Parser = Parsec String ()

program :: Parser (Program SourcePos)
program = ws *> (Program <$> programPreamble <*> expr <* eof)

programPreamble :: Parser [(Var,SourcePos)]
programPreamble = choice
  [ (kwInput *> many1 preambleVar <* kwIn)
  , return []
  ] <?> "program preamble"

preambleVar :: Parser (Var,SourcePos)
preambleVar =
  (\ pos x -> (x,pos)) <$> getPosition <*> ident

expr :: Parser (Expr SourcePos)
expr = choice
  [ letExpr
  , ifExpr
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

opExpr :: Parser (Expr SourcePos)
opExpr =
  opLeftAssoc  opOr             $
  opLeftAssoc  opAnd            $
  opUnary      opNot            $
  opNoAssoc    opCmp            $
  opLeftAssoc  opAdditive       $
  opLeftAssoc  opMultiplicative $
  opUnary      opUMinus         $
  simpleExpr
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

simpleExpr :: Parser (Expr SourcePos)
simpleExpr = choice
  [ setPos <$> getPosition <*> parens tokenParser expr
  , ENum   <$> getPosition <*> natural tokenParser
  , EBool  <$> getPosition <*> boolConst
  , EVar   <$> getPosition <*> ident
  ]

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
parseExpr :: SourceName -> String -> Either ParseError (Expr SourcePos)
parseExpr = parse expr
    
parseProgram :: SourceName -> String -> Either ParseError (Program SourcePos)
parseProgram = parse program
