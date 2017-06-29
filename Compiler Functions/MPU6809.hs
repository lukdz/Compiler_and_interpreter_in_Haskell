{-# LANGUAGE Safe #-}
module MPU6809 where

import Data.Word
import Data.Int
import Data.List (intercalate)

type Label = Int

data Register
  = RegD | RegX | RegY | RegU | RegS | RegPC | RegA | RegB | RegCC | RegDP
  deriving (Eq, Show, Read)

data IndexRegister
  = IxX | IxY | IxU | IxS
  deriving (Eq, Show, Read)

data IndexedAddr
  = IA_Index Int16 IndexRegister      -- n, R
  | IA_IndexA IndexRegister           -- A, R
  | IA_IndexB IndexRegister           -- B, R
  | IA_IndexD IndexRegister           -- D, R
  | IA_Incr1 IndexRegister            -- , R+
  | IA_Incr2 IndexRegister            -- , R++
  | IA_Decr1 IndexRegister            -- , -R
  | IA_Decr2 IndexRegister            -- , --R
  | IA_Label Int16 Label              -- n+l
  | IA_PCOff Int16                    -- n, PCR
  | IA_IndirIndex Int16 IndexRegister -- [n, R]
  | IA_IndirIndexA IndexRegister      -- [A, R]
  | IA_IndirIndexB IndexRegister      -- [B, R]
  | IA_IndirIndexD IndexRegister      -- [D, R]
  | IA_IndirIncr2  IndexRegister      -- [,R++]
  | IA_IndirDecr2  IndexRegister      -- [,--R]
  | IA_IndirLabel Int16 Label         -- [n+l]
  | IA_IndirPCOff Int16               -- [n, PCR]
  | IA_IndirExtended Word16           -- [n]
  deriving (Eq, Show, Read)

data Addr
  = ADirect   Word8
  | AIndexed  IndexedAddr
  | AExtended Word16
  deriving (Eq, Show, Read)

data AddrImm8
  = Imm8  Word8
  | Addr8 Addr
  deriving (Eq, Show, Read)

data AddrImm16
  = Imm16  Word16
  | Addr16 Addr
  deriving (Eq, Show, Read)

data Rel8
  = R8Label Label
  | R8Imm   Int8
  deriving (Eq, Show, Read)

data Rel16
  = R16Label Label
  | R16Imm   Int16
  deriving (Eq, Show, Read)

data Instr
  = Label Label
  | Asciiz String
  | ABX
  | ADCA AddrImm8
  | ADCB AddrImm8
  | ADDA AddrImm8
  | ADDB AddrImm8
  | ADDD AddrImm16
  | ANDA AddrImm8
  | ANDB AddrImm8
  | ANDCC Word8
  | ASLA
  | ASLB
  | ASL  Addr
  | ASRA
  | ASRB
  | ASR  Addr
  | BCC  Rel8
  | BCS  Rel8
  | BEQ  Rel8
  | BGE  Rel8
  | BGT  Rel8
  | BHI  Rel8
  | BHS  Rel8
  | BITA AddrImm8
  | BITB AddrImm8
  | BLE  Rel8
  | BLO  Rel8
  | BLS  Rel8
  | BLT  Rel8
  | BMI  Rel8
  | BNE  Rel8
  | BPL  Rel8
  | BRA  Rel8
  | BRN  Rel8
  | BSR  Rel8
  | BVC  Rel8
  | BVS  Rel8
  | CLRA
  | CLRB
  | CLR  Addr
  | CMPA AddrImm8
  | CMPB AddrImm8
  | CMPD AddrImm16
  | CMPS AddrImm16
  | CMPU AddrImm16
  | CMPX AddrImm16
  | CMPY AddrImm16
  | COMA
  | COMB
  | COM  Addr
  | CWAI Word8
  | DAA
  | DECA
  | DECB
  | DEC  Addr
  | EORA AddrImm8
  | EORB AddrImm8
  | EXG  Register Register
  | INCA
  | INCB
  | INC  Addr
  | JMP  Addr
  | JSR  Addr
  | LBCC Rel16
  | LBCS Rel16
  | LBEQ Rel16
  | LBGE Rel16
  | LBGT Rel16
  | LBHI Rel16
  | LBHS Rel16
  | LBLE Rel16
  | LBLO Rel16
  | LBLS Rel16
  | LBLT Rel16
  | LBMI Rel16
  | LBNE Rel16
  | LBPL Rel16
  | LBRA Rel16
  | LBRN Rel16
  | LBSR Rel16
  | LBVC Rel16
  | LBVS Rel16
  | LDA  AddrImm8
  | LDB  AddrImm8
  | LDD  AddrImm16
  | LDS  AddrImm16
  | LDU  AddrImm16
  | LDX  AddrImm16
  | LDY  AddrImm16
  | LEAS IndexedAddr
  | LEAU IndexedAddr
  | LEAX IndexedAddr
  | LEAY IndexedAddr
  | LSLA
  | LSLB
  | LSL  Addr
  | LSRA
  | LSRB
  | LSR  Addr
  | MUL
  | NEGA
  | NEGB
  | NEG  Addr
  | NOP
  | ORA  AddrImm8
  | ORB  AddrImm8
  | ORCC Word8
  | PSHS [Register]
  | PSHU [Register]
  | PULS [Register]
  | PULU [Register]
  | ROLA
  | ROLB
  | ROL  Addr
  | RORA
  | RORB
  | ROR  Addr
  | RTI
  | RTS
  | SBCA AddrImm8
  | SBCB AddrImm8
  | SEX
  | STA  Addr
  | STB  Addr
  | STD  Addr
  | STS  Addr
  | STU  Addr
  | STX  Addr
  | STY  Addr
  | SUBA AddrImm8
  | SUBB AddrImm8
  | SUBD AddrImm16
  | SWI
  | SWI2
  | SWI3
  | SYNC
  | TFR  Register Register
  | TSTA
  | TSTB
  | TST  Addr
  deriving (Eq, Show, Read)

-- ============================================================================

class HasImm8 a where
  imm8 :: Integral x => x -> a

instance HasImm8 AddrImm8 where
  imm8 = Imm8 . fromIntegral

class HasImm16 a where
  imm16 :: Integral x => x -> a

instance HasImm16 AddrImm16 where
  imm16 = Imm16 . fromIntegral

class HasIndexed a where
  index :: Integral x => x -> IndexRegister -> a
  incr1 :: IndexRegister -> a
  incr2 :: IndexRegister -> a
  decr2 :: IndexRegister -> a

instance HasIndexed IndexedAddr where
  index x r = IA_Index (fromIntegral x) r
  incr1     = IA_Incr1
  incr2     = IA_Incr2
  decr2     = IA_Decr2

instance HasIndexed Addr where
  index x r = AIndexed$ index x r
  incr1     = AIndexed . incr1
  incr2     = AIndexed . incr2
  decr2     = AIndexed . decr2 

instance HasIndexed AddrImm8 where
  index x r = Addr8$ index x r
  incr1     = Addr8 . incr1
  incr2     = Addr8 . incr2
  decr2     = Addr8 . decr2

instance HasIndexed AddrImm16 where
  index x r = Addr16$ index x r
  incr1     = Addr16 . incr1
  incr2     = Addr16 . incr2
  decr2     = Addr16 . decr2

-- ============================================================================

prettyLabel :: Label -> String
prettyLabel l = "lbl_" ++ show l

prettyRegister :: Register -> String
prettyRegister RegD  = "D"
prettyRegister RegX  = "X"
prettyRegister RegY  = "Y"
prettyRegister RegU  = "U"
prettyRegister RegS  = "S"
prettyRegister RegPC = "PC"
prettyRegister RegA  = "A"
prettyRegister RegB  = "B"
prettyRegister RegCC = "CC"
prettyRegister RegDP = "DP"

prettyIndexRegister :: IndexRegister -> String
prettyIndexRegister IxX = "X"
prettyIndexRegister IxY = "Y"
prettyIndexRegister IxU = "U"
prettyIndexRegister IxS = "S"

prettyIndexedAddr :: IndexedAddr -> String
prettyIndexedAddr (IA_Index 0 r) = ", " ++ prettyIndexRegister r
prettyIndexedAddr (IA_Index n r) = show n ++ ", " ++ prettyIndexRegister r
prettyIndexedAddr (IA_IndexA r)  = "A, " ++ prettyIndexRegister r
prettyIndexedAddr (IA_IndexB r)  = "B, " ++ prettyIndexRegister r
prettyIndexedAddr (IA_IndexD r)  = "D, " ++ prettyIndexRegister r
prettyIndexedAddr (IA_Incr1 r)   = "," ++ prettyIndexRegister r ++ "+"
prettyIndexedAddr (IA_Incr2 r)   = "," ++ prettyIndexRegister r ++ "++"
prettyIndexedAddr (IA_Decr1 r)   = ",-" ++ prettyIndexRegister r
prettyIndexedAddr (IA_Decr2 r)   = ",--" ++ prettyIndexRegister r
prettyIndexedAddr (IA_Label 0 l) = prettyLabel l ++ ", PCR"
prettyIndexedAddr (IA_Label n l) = show n ++ "+" ++ prettyLabel l ++ ", PCR"
prettyIndexedAddr (IA_PCOff n)   = show n ++ "+ *, PCR"
prettyIndexedAddr (IA_IndirIndex n r) =
  "[" ++ show n ++ ", " ++ prettyIndexRegister r ++ "]"
prettyIndexedAddr (IA_IndirIndexA r)  = "[A, " ++ prettyIndexRegister r ++ "]"
prettyIndexedAddr (IA_IndirIndexB r)  = "[B, " ++ prettyIndexRegister r ++ "]"
prettyIndexedAddr (IA_IndirIndexD r)  = "[D, " ++ prettyIndexRegister r ++ "]"
prettyIndexedAddr (IA_IndirIncr2 r)   = "[," ++ prettyIndexRegister r ++ "++]"
prettyIndexedAddr (IA_IndirDecr2 r)   = "[,--" ++ prettyIndexRegister r ++ "]"
prettyIndexedAddr (IA_IndirLabel 0 l) = "[" ++ prettyLabel l ++ ", PCR]"
prettyIndexedAddr (IA_IndirLabel n l) =
  "[" ++ show n ++ "+" ++ prettyLabel l ++ ", PCR]"
prettyIndexedAddr (IA_IndirPCOff n)   = "[" ++ show n ++ " + *, PCR]"
prettyIndexedAddr (IA_IndirExtended n) = "[" ++ show n ++ "]"

prettyAddr :: Addr -> String
prettyAddr (ADirect n)   = "< " ++ show n
prettyAddr (AIndexed a)  = prettyIndexedAddr a
prettyAddr (AExtended n) = show n

prettyAddrImm8 :: AddrImm8 -> String
prettyAddrImm8 (Imm8 x)  = "#" ++ show x
prettyAddrImm8 (Addr8 a) = prettyAddr a

prettyAddrImm16 :: AddrImm16 -> String
prettyAddrImm16 (Imm16  x) = "#" ++ show x
prettyAddrImm16 (Addr16 a) = prettyAddr a

prettyRel8 :: Rel8 -> String
prettyRel8 (R8Label l) = prettyLabel l
prettyRel8 (R8Imm n)   = "#" ++ show n

prettyRel16 :: Rel16 -> String
prettyRel16 (R16Label l) = prettyLabel l
prettyRel16 (R16Imm n)   = "#" ++ show n

prettyInstr :: Instr -> String
prettyInstr (Label l)   = prettyLabel l
prettyInstr (Asciiz s)  = "    FCB       " ++ show s ++ ", 0"
prettyInstr ABX         = "    ABX"
prettyInstr (ADCA x)    = "    ADCA      " ++ prettyAddrImm8  x
prettyInstr (ADCB x)    = "    ADCB      " ++ prettyAddrImm8  x
prettyInstr (ADDA x)    = "    ADDA      " ++ prettyAddrImm8  x
prettyInstr (ADDB x)    = "    ADDB      " ++ prettyAddrImm8  x
prettyInstr (ADDD x)    = "    ADDD      " ++ prettyAddrImm16 x
prettyInstr (ANDA x)    = "    ANDA      " ++ prettyAddrImm8  x
prettyInstr (ANDB x)    = "    ANDB      " ++ prettyAddrImm8  x
prettyInstr (ANDCC n)   = "    ANDCC     #" ++ show n
prettyInstr ASLA        = "    ASLA"
prettyInstr ASLB        = "    ASLB"
prettyInstr (ASL a)     = "    ASL       " ++ prettyAddr a
prettyInstr ASRA        = "    ASRA"
prettyInstr ASRB        = "    ASRB"
prettyInstr (ASR a)     = "    ASR       " ++ prettyAddr a
prettyInstr (BCC r)     = "    BCC       " ++ prettyRel8 r
prettyInstr (BCS r)     = "    BCS       " ++ prettyRel8 r
prettyInstr (BEQ r)     = "    BEQ       " ++ prettyRel8 r
prettyInstr (BGE r)     = "    BGE       " ++ prettyRel8 r
prettyInstr (BGT r)     = "    BGT       " ++ prettyRel8 r
prettyInstr (BHI r)     = "    BHI       " ++ prettyRel8 r
prettyInstr (BHS r)     = "    BHS       " ++ prettyRel8 r
prettyInstr (BITA x)    = "    BITA      " ++ prettyAddrImm8 x
prettyInstr (BITB x)    = "    BITB      " ++ prettyAddrImm8 x
prettyInstr (BLE r)     = "    BLE       " ++ prettyRel8 r
prettyInstr (BLO r)     = "    BLO       " ++ prettyRel8 r
prettyInstr (BLS r)     = "    BLS       " ++ prettyRel8 r
prettyInstr (BLT r)     = "    BLT       " ++ prettyRel8 r
prettyInstr (BMI r)     = "    BMI       " ++ prettyRel8 r
prettyInstr (BNE r)     = "    BNE       " ++ prettyRel8 r
prettyInstr (BPL r)     = "    BPL       " ++ prettyRel8 r
prettyInstr (BRA r)     = "    BRA       " ++ prettyRel8 r
prettyInstr (BRN r)     = "    BRN       " ++ prettyRel8 r
prettyInstr (BSR r)     = "    BSR       " ++ prettyRel8 r
prettyInstr (BVC r)     = "    BVC       " ++ prettyRel8 r
prettyInstr (BVS r)     = "    BVS       " ++ prettyRel8 r
prettyInstr CLRA        = "    CLRA"
prettyInstr CLRB        = "    CLRB"
prettyInstr (CLR  a)    = "    CLR       " ++ prettyAddr a
prettyInstr (CMPA x)    = "    CMPA      " ++ prettyAddrImm8  x
prettyInstr (CMPB x)    = "    CMPB      " ++ prettyAddrImm8  x
prettyInstr (CMPD x)    = "    CMPD      " ++ prettyAddrImm16 x
prettyInstr (CMPS x)    = "    CMPS      " ++ prettyAddrImm16 x
prettyInstr (CMPU x)    = "    CMPU      " ++ prettyAddrImm16 x
prettyInstr (CMPX x)    = "    CMPX      " ++ prettyAddrImm16 x
prettyInstr (CMPY x)    = "    CMPY      " ++ prettyAddrImm16 x
prettyInstr COMA        = "    COMA"
prettyInstr COMB        = "    COMB"
prettyInstr (COM  a)    = "    COM       " ++ prettyAddr a
prettyInstr (CWAI n)    = "    CWAI      #" ++ show n
prettyInstr DAA         = "    DAA"
prettyInstr DECA        = "    DECA"
prettyInstr DECB        = "    DECB"
prettyInstr (DEC  a)    = "    DEC       " ++ prettyAddr a
prettyInstr (EORA x)    = "    EORA      " ++ prettyAddrImm8 x
prettyInstr (EORB x)    = "    EORB      " ++ prettyAddrImm8 x
prettyInstr (EXG r0 r1) =
  "    EXG       " ++ prettyRegister r0 ++ ", " ++ prettyRegister r1
prettyInstr INCA        = "    INCA"
prettyInstr INCB        = "    INCB"
prettyInstr (INC  a)    = "    INC       " ++ prettyAddr a
prettyInstr (JMP  a)    = "    JMP       " ++ prettyAddr a
prettyInstr (JSR  a)    = "    JSR       " ++ prettyAddr a
prettyInstr (LBCC r)    = "    LBCC      " ++ prettyRel16 r
prettyInstr (LBCS r)    = "    LBCS      " ++ prettyRel16 r
prettyInstr (LBEQ r)    = "    LBEQ      " ++ prettyRel16 r
prettyInstr (LBGE r)    = "    LBGE      " ++ prettyRel16 r
prettyInstr (LBGT r)    = "    LBGT      " ++ prettyRel16 r
prettyInstr (LBHI r)    = "    LBHI      " ++ prettyRel16 r
prettyInstr (LBHS r)    = "    LBHS      " ++ prettyRel16 r
prettyInstr (LBLE r)    = "    LBLE      " ++ prettyRel16 r
prettyInstr (LBLO r)    = "    LBLO      " ++ prettyRel16 r
prettyInstr (LBLS r)    = "    LBLS      " ++ prettyRel16 r
prettyInstr (LBLT r)    = "    LBLT      " ++ prettyRel16 r
prettyInstr (LBMI r)    = "    LBMI      " ++ prettyRel16 r
prettyInstr (LBNE r)    = "    LBNE      " ++ prettyRel16 r
prettyInstr (LBPL r)    = "    LBPL      " ++ prettyRel16 r
prettyInstr (LBRA r)    = "    LBRA      " ++ prettyRel16 r
prettyInstr (LBRN r)    = "    LBRN      " ++ prettyRel16 r
prettyInstr (LBSR r)    = "    LBSR      " ++ prettyRel16 r
prettyInstr (LBVC r)    = "    LBVC      " ++ prettyRel16 r
prettyInstr (LBVS r)    = "    LBVS      " ++ prettyRel16 r
prettyInstr (LDA x)     = "    LDA       " ++ prettyAddrImm8  x
prettyInstr (LDB x)     = "    LDB       " ++ prettyAddrImm8  x
prettyInstr (LDD x)     = "    LDD       " ++ prettyAddrImm16 x
prettyInstr (LDS x)     = "    LDS       " ++ prettyAddrImm16 x
prettyInstr (LDU x)     = "    LDU       " ++ prettyAddrImm16 x
prettyInstr (LDX x)     = "    LDX       " ++ prettyAddrImm16 x
prettyInstr (LDY x)     = "    LDY       " ++ prettyAddrImm16 x
prettyInstr (LEAS a)    = "    LEAS      " ++ prettyIndexedAddr a
prettyInstr (LEAU a)    = "    LEAU      " ++ prettyIndexedAddr a
prettyInstr (LEAX a)    = "    LEAX      " ++ prettyIndexedAddr a
prettyInstr (LEAY a)    = "    LEAY      " ++ prettyIndexedAddr a
prettyInstr LSLA        = "    LSLA"
prettyInstr LSLB        = "    LSLB"
prettyInstr (LSL a)     = "    LSL       " ++ prettyAddr a
prettyInstr LSRA        = "    LSRA"
prettyInstr LSRB        = "    LSRB"
prettyInstr (LSR a)     = "    LSR       " ++ prettyAddr a
prettyInstr MUL         = "    MUL"
prettyInstr NEGA        = "    NEGA"
prettyInstr NEGB        = "    NEGB"
prettyInstr (NEG a)     = "    NEG       " ++ prettyAddr a
prettyInstr NOP         = "    NOP"
prettyInstr (ORA x)     = "    ORA       " ++ prettyAddrImm8  x
prettyInstr (ORB x)     = "    ORB       " ++ prettyAddrImm8  x
prettyInstr (ORCC n)    = "    ORCC      #" ++ show n
prettyInstr (PSHS rs)   =
  "    PSHS      " ++ (intercalate "," $ map prettyRegister rs)
prettyInstr (PSHU rs)   =
  "    PSHU      " ++ (intercalate "," $ map prettyRegister rs)
prettyInstr (PULS rs)   =
  "    PULS      " ++ (intercalate "," $ map prettyRegister rs)
prettyInstr (PULU rs)   =
  "    PULU      " ++ (intercalate "," $ map prettyRegister rs)
prettyInstr ROLA        = "    ROLA"
prettyInstr ROLB        = "    ROLB"
prettyInstr (ROL a)     = "    ROL       " ++ prettyAddr a
prettyInstr RORA        = "    RORA"
prettyInstr RORB        = "    RORB"
prettyInstr (ROR a)     = "    ROR       " ++ prettyAddr a
prettyInstr RTI         = "    RTI"
prettyInstr RTS         = "    RTS"
prettyInstr (SBCA x)    = "    SBCA      " ++ prettyAddrImm8  x
prettyInstr (SBCB x)    = "    SBCB      " ++ prettyAddrImm8  x
prettyInstr SEX         = "    SEX"
prettyInstr (STA a)     = "    STA       " ++ prettyAddr a
prettyInstr (STB a)     = "    STB       " ++ prettyAddr a
prettyInstr (STD a)     = "    STD       " ++ prettyAddr a
prettyInstr (STS a)     = "    STS       " ++ prettyAddr a
prettyInstr (STU a)     = "    STU       " ++ prettyAddr a
prettyInstr (STX a)     = "    STX       " ++ prettyAddr a
prettyInstr (STY a)     = "    STY       " ++ prettyAddr a
prettyInstr (SUBA x)    = "    SUBA      " ++ prettyAddrImm8  x
prettyInstr (SUBB x)    = "    SUBB      " ++ prettyAddrImm8  x
prettyInstr (SUBD x)    = "    SUBD      " ++ prettyAddrImm16 x
prettyInstr SWI         = "    SWI"
prettyInstr SWI2        = "    SWI2"
prettyInstr SWI3        = "    SWI3"
prettyInstr SYNC        = "    SYNC"
prettyInstr (TFR r0 r1) =
  "    TFR       " ++ prettyRegister r0 ++ ", " ++ prettyRegister r1
prettyInstr TSTA        = "    TSTA"
prettyInstr TSTB        = "    TSTB"
prettyInstr (TST a)     = "    TST       " ++ prettyAddr a
