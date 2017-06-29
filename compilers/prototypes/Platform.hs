{-# LANGUAGE Safe #-}
module Platform(
  sys_HALT, sys_ALLOC, sys_MUL, sys_DIV,
  sys_PUTC, sys_PUTSTR, sys_PUTNUM,
  sys_GETC, sys_GETSTR, sys_GETNUM,
  preamble) where

import Data.Word
import MPU6809

import qualified Data.Set as Set

sys_HALT :: Word8
sys_HALT   = 0

sys_ALLOC :: Word8
sys_ALLOC  = 1

sys_MUL :: Word8
sys_MUL    = 2

sys_DIV :: Word8
sys_DIV    = 3

sys_PUTC :: Word8
sys_PUTC   = 8

sys_PUTSTR :: Word8
sys_PUTSTR = 9

sys_PUTNUM :: Word8
sys_PUTNUM = 10

sys_GETC :: Word8
sys_GETC   = 12

sys_GETSTR :: Word8
sys_GETSTR = 13

sys_GETNUM :: Word8
sys_GETNUM = 14

preamble :: [String] -> [Instr] -> [Instr]
preamble vars is =
  prolog ++ is ++ epilog
  where
    (prolog, epilog) = genPreamble fr vars
    fr = freshLabels 0 $ labelsOf is

labelsOf :: [Instr] -> Set.Set Label
labelsOf =
  flip foldr Set.empty $ \ i ->
    case i of
      Label l -> Set.insert l
      _       -> id

freshLabels :: Label -> Set.Set Label -> [Label]
freshLabels l s =
  if Set.member l s then freshLabels (l+1) s
  else l:freshLabels (l+1) s

genPreamble :: [Label] -> [String] -> ([Instr], [Instr])
genPreamble fr []  =
  let (l:_) = fr in
  (callPrintAndHalt l ++ [ Label l ], [])
genPreamble fr0 vars = (prolog, epilog)
  where
    (codeLabel:eqLabel:fr) = fr0
    prolog =
      [ LEAU$ index (-2*n) IxU ] -- Alloc vars 
      ++ readVarsCode
      ++ callPrintAndHalt codeLabel
      ++ [ Label codeLabel ]
    epilog = varNameConstants ++ eqConstant

    n                = length vars
    readVarsCode     = concat $ map fst varCode
    varNameConstants = concat $ map snd varCode
    eqConstant       = [ Label eqLabel, Asciiz " = " ]
    varCode          = zipWith3 procVar fr vars [0..]
    procVar l x i    = (readVarCode l i, [ Label l, Asciiz x ])
    readVarCode l i  =
      [ LEAX$ IA_Label 0 l
      , STX$  decr2 IxU
      , LDB$  imm8 sys_PUTSTR
      , SWI2
      , LEAX$ IA_Label 0 eqLabel
      , STX$  decr2 IxU
      , LDB$  imm8 sys_PUTSTR
      , SWI2
      , LDB$  imm8 sys_GETNUM
      , SWI2
      , STD$  index (2*i) IxU
      ]

callPrintAndHalt :: Label -> [Instr]
callPrintAndHalt l =
  [ BSR$ R8Label l
  , STD$ decr2 IxU
  , LDB$ imm8 sys_PUTNUM
  , SWI2
  , LDD$  imm16 (0x0A00 + fromIntegral sys_PUTC :: Int)
  , SWI2
  , CLRB
  , SWI2
  ]
