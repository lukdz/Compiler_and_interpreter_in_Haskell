{-# LANGUAGE Safe #-}
module MacroAsm where

import Platform
import qualified MPU6809

type Label = MPU6809.Label

data MCondition
  = MC_1
  | MC_0
  | MC_Z
  | MC_NZ
  | MC_P
  | MC_NP
  | MC_N
  | MC_NN
  | MC_EQ
  | MC_NE
  | MC_LT
  | MC_GE
  | MC_LE
  | MC_GT
  deriving (Eq, Show, Read)

data MInstr
  = MLabel Label
  | MInstr MPU6809.Instr
  | MConst Integer
  | MGetLabel Label
  | MGetLocal Int
  | MSetLocal Int
  | MPush
  | MPopN Int
  | MPopAcc
  | MNot
  | MNeg
  | MAnd
  | MOr
  | MAdd
  | MSub
  | MMul
  | MDiv
  | MMod
  | MAlloc Int
  | MGet   Int
  | MSet   Int
  | MBranch MCondition Label
  | MJump Label
  | MJumpAcc
  | MCall Label
  | MCallAcc
  | MRet
  deriving (Eq, Show, Read)

-- ============================================================================

negMCondition :: MCondition -> MCondition
negMCondition MC_1  = MC_0
negMCondition MC_0  = MC_1
negMCondition MC_Z  = MC_NZ
negMCondition MC_NZ = MC_Z
negMCondition MC_P  = MC_NP
negMCondition MC_NP = MC_P
negMCondition MC_N  = MC_NN
negMCondition MC_NN = MC_N
negMCondition MC_EQ = MC_NE
negMCondition MC_NE = MC_EQ
negMCondition MC_LT = MC_GE
negMCondition MC_GE = MC_LT
negMCondition MC_LE = MC_GT
negMCondition MC_GT = MC_LE

prettyMCondition :: MCondition -> String
prettyMCondition MC_1  = "1 "
prettyMCondition MC_0  = "0 "
prettyMCondition MC_Z  = "Z "
prettyMCondition MC_NZ = "NZ"
prettyMCondition MC_P  = "P "
prettyMCondition MC_NP = "NP"
prettyMCondition MC_N  = "N "
prettyMCondition MC_NN = "NN"
prettyMCondition MC_EQ = "EQ"
prettyMCondition MC_NE = "NE"
prettyMCondition MC_LT = "LT"
prettyMCondition MC_GE = "GE"
prettyMCondition MC_LE = "LE"
prettyMCondition MC_GT = "GT"

prettyMInstr :: MInstr -> String
prettyMInstr (MLabel l)    = MPU6809.prettyLabel l
prettyMInstr (MInstr i)    = MPU6809.prettyInstr i
prettyMInstr (MConst n)    = "    MConst    " ++ show n
prettyMInstr (MGetLabel l) = "    MGetLabel " ++ MPU6809.prettyLabel l
prettyMInstr (MGetLocal n) = "    MGetLocal " ++ show n
prettyMInstr (MSetLocal n) = "    MSetLocal " ++ show n
prettyMInstr MPush         = "    MPush"
prettyMInstr (MPopN n)     = "    MPopN     " ++ show n
prettyMInstr MPopAcc       = "    MPopAcc"
prettyMInstr MNot          = "    MNot"
prettyMInstr MNeg          = "    MNeg"
prettyMInstr MAnd          = "    MAnd"
prettyMInstr MOr           = "    MOr"
prettyMInstr MAdd          = "    MAdd"
prettyMInstr MSub          = "    MSub"
prettyMInstr MMul          = "    MMul"
prettyMInstr MDiv          = "    MDiv"
prettyMInstr MMod          = "    MMod"
prettyMInstr (MAlloc n)    = "    MAlloc    " ++ show n
prettyMInstr (MGet n)      = "    MGet      " ++ show n
prettyMInstr (MSet n)      = "    MSet      " ++ show n
prettyMInstr (MBranch c l) =
  "    MBranch   " ++ prettyMCondition c ++ "  " ++ MPU6809.prettyLabel l
prettyMInstr (MJump l)     = "    MJump     " ++ MPU6809.prettyLabel l
prettyMInstr MJumpAcc      = "    MJumpAcc"
prettyMInstr (MCall l)     = "    MCall     " ++ MPU6809.prettyLabel l
prettyMInstr MCallAcc      = "    MCallAcc"
prettyMInstr MRet          = "    MRet"

-- ============================================================================

unfoldMInstr :: MInstr -> [MPU6809.Instr]
unfoldMInstr (MLabel l)    = [MPU6809.Label l]
unfoldMInstr (MInstr i)    = [i]
unfoldMInstr (MConst n)    = [MPU6809.LDD$ MPU6809.imm16 n]
unfoldMInstr (MGetLabel l) =
  [ MPU6809.LEAX$ MPU6809.IA_Label 0 l
  , MPU6809.TFR   MPU6809.RegX  MPU6809.RegD
  ]
unfoldMInstr (MGetLocal n) = [ MPU6809.LDD$  MPU6809.index (2*n) MPU6809.IxU ]
unfoldMInstr (MSetLocal n) = [ MPU6809.STD$  MPU6809.index (2*n) MPU6809.IxU ]
unfoldMInstr MPush         = [ MPU6809.STD$  MPU6809.decr2 MPU6809.IxU ]
unfoldMInstr (MPopN n)     = [ MPU6809.LEAU$ MPU6809.index (2*n) MPU6809.IxU ]
unfoldMInstr MPopAcc       = [ MPU6809.LDD$  MPU6809.incr2 MPU6809.IxU ]
unfoldMInstr MNot          = [ MPU6809.COMA, MPU6809.COMB ]
unfoldMInstr MNeg          =
  [ MPU6809.COMA
  , MPU6809.COMB
  , MPU6809.ADDD$ MPU6809.imm16 1
  ]
unfoldMInstr MAnd =
  [ MPU6809.ANDA$ MPU6809.incr1 MPU6809.IxU
  , MPU6809.ANDB$ MPU6809.incr1 MPU6809.IxU
  ]
unfoldMInstr MOr  =
  [ MPU6809.ORA$ MPU6809.incr1 MPU6809.IxU
  , MPU6809.ORB$ MPU6809.incr1 MPU6809.IxU
  ]
unfoldMInstr MAdd = [ MPU6809.ADDD$ MPU6809.incr2 MPU6809.IxU ]
unfoldMInstr MSub =
  [ MPU6809.STD$  MPU6809.decr2 MPU6809.IxS
  , MPU6809.LDD$  MPU6809.incr2 MPU6809.IxU
  , MPU6809.SUBD$ MPU6809.incr2 MPU6809.IxS
  ]
unfoldMInstr MMul =
  [ MPU6809.STD$  MPU6809.decr2 MPU6809.IxU
  , MPU6809.LDB$  MPU6809.imm8 sys_MUL
  , MPU6809.SWI2
  ]
unfoldMInstr MDiv =
  [ MPU6809.STD$  MPU6809.decr2 MPU6809.IxU
  , MPU6809.LDB$  MPU6809.imm8 sys_DIV
  , MPU6809.SWI2
  , MPU6809.LEAU$ MPU6809.index 2 MPU6809.IxU
  ]
unfoldMInstr MMod =
  [ MPU6809.STD$  MPU6809.decr2 MPU6809.IxU
  , MPU6809.LDB$  MPU6809.imm8 sys_DIV
  , MPU6809.SWI2
  , MPU6809.LDD$  MPU6809.incr2 MPU6809.IxU
  ]
unfoldMInstr (MAlloc n) =
  [ MPU6809.LDD$  MPU6809.imm16 (n * 256 + fromIntegral sys_ALLOC)
  , MPU6809.SWI2
  ]
unfoldMInstr (MGet n) =
  [ MPU6809.TFR  MPU6809.RegD  MPU6809.RegX
  , MPU6809.LDD$ MPU6809.index (2*n) MPU6809.IxX
  ]
unfoldMInstr (MSet n) =
  [ MPU6809.LDX$ MPU6809.index 0 MPU6809.IxU
  , MPU6809.STD$ MPU6809.index (2*n) MPU6809.IxX
  ]
unfoldMInstr (MBranch MC_1  l) = [ MPU6809.LBRA$ MPU6809.R16Label l ]
unfoldMInstr (MBranch MC_0  _) = []
unfoldMInstr (MBranch MC_Z  l) =
  [ MPU6809.CMPD$ MPU6809.imm16 0
  , MPU6809.LBEQ$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_NZ l) =
  [ MPU6809.CMPD$ MPU6809.imm16 0
  , MPU6809.LBNE$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_P  l) =
  [ MPU6809.CMPD$ MPU6809.imm16 0
  , MPU6809.LBGT$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_NP l) =
  [ MPU6809.CMPD$ MPU6809.imm16 0
  , MPU6809.LBLE$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_N  l) =
  [ MPU6809.CMPD$ MPU6809.imm16 0
  , MPU6809.LBLT$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_NN l) =
  [ MPU6809.CMPD$ MPU6809.imm16 0
  , MPU6809.LBGE$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_EQ l) =
  [ MPU6809.CMPD$ MPU6809.incr2 MPU6809.IxU
  , MPU6809.LBEQ$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_NE l) =
  [ MPU6809.CMPD$ MPU6809.incr2 MPU6809.IxU
  , MPU6809.LBNE$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_LT l) =
  [ MPU6809.CMPD$ MPU6809.incr2 MPU6809.IxU
  , MPU6809.LBGT$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_GE l) =
  [ MPU6809.CMPD$ MPU6809.incr2 MPU6809.IxU
  , MPU6809.LBLE$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_LE l) =
  [ MPU6809.CMPD$ MPU6809.incr2 MPU6809.IxU
  , MPU6809.LBGE$ MPU6809.R16Label l
  ]
unfoldMInstr (MBranch MC_GT l) =
  [ MPU6809.CMPD$ MPU6809.incr2 MPU6809.IxU
  , MPU6809.LBLT$ MPU6809.R16Label l
  ]
unfoldMInstr (MJump l) = [ MPU6809.LBRA$ MPU6809.R16Label l ]
unfoldMInstr MJumpAcc  =
  [ MPU6809.TFR  MPU6809.RegD  MPU6809.RegX
  , MPU6809.JMP$ MPU6809.index 0 MPU6809.IxX
  ]
unfoldMInstr (MCall l) = [ MPU6809.LBSR$ MPU6809.R16Label l ]
unfoldMInstr MCallAcc  =
  [ MPU6809.TFR  MPU6809.RegD  MPU6809.RegX
  , MPU6809.JSR$ MPU6809.index 0 MPU6809.IxX
  ]
unfoldMInstr MRet = [ MPU6809.RTS ]
