{-# LANGUAGE Safe #-}
module Assembler(assembly) where

import MPU6809
import Data.Word
import Data.Bits

import Data.ByteString.Lazy (ByteString)
import Data.Char (ord)

import qualified Data.ByteString.Builder as BS
import qualified Data.Map as Map
import qualified Control.Monad.State as ST

data AssemblerState = AssemblerState
  { asBytes    :: [BS.Builder]
  , asPos      :: Int
  , asLabels   :: Map.Map Label Int
  , asLabelMap :: Map.Map Label Int
  }

assembly :: [Instr] -> ByteString
assembly code = BS.toLazyByteString $ mconcat $ reverse bs
  where
    AssemblerState bs _ lmap _ = ST.execState (mapM_ genCode code) initState
    initState =
      AssemblerState
      { asBytes    = []
      , asPos      = 0
      , asLabels   = Map.empty
      , asLabelMap = lmap
      }

genCode :: Instr -> ST.State AssemblerState ()
genCode (Label l)   = setLabel l
genCode (Asciiz s)  = mapM_ (byte . fromIntegral . ord) s >> byte 0
genCode ABX         = byte 0x3A
genCode (ADCA x)    = addrImm8  0x89 0x99 0xA9 0xB9 x
genCode (ADCB x)    = addrImm8  0xC9 0xD9 0xE9 0xF9 x
genCode (ADDA x)    = addrImm8  0x8B 0x9B 0xAB 0xBB x
genCode (ADDB x)    = addrImm8  0xCB 0xDB 0xEB 0xFB x
genCode (ADDD x)    = addrImm16 0xC3 0xD3 0xE3 0xF3 x
genCode (ANDA x)    = addrImm8  0x84 0x94 0xA4 0xB4 x
genCode (ANDB x)    = addrImm8  0xC4 0xD4 0xE4 0xF4 x
genCode (ANDCC n)   = byte 0x1C >> byte n
genCode ASLA        = byte 0x48
genCode ASLB        = byte 0x58
genCode (ASL a)     = addr 0x08 0x68 0x78 a
genCode ASRA        = byte 0x47
genCode ASRB        = byte 0x57
genCode (ASR a)     = addr 0x07 0x67 0x77 a
genCode (BCC r)     = byte 0x24 >> rel8 r
genCode (BCS r)     = byte 0x25 >> rel8 r
genCode (BEQ r)     = byte 0x27 >> rel8 r
genCode (BGE r)     = byte 0x2C >> rel8 r
genCode (BGT r)     = byte 0x2E >> rel8 r
genCode (BHI r)     = byte 0x22 >> rel8 r
genCode (BHS r)     = byte 0x24 >> rel8 r
genCode (BITA x)    = addrImm8 0x85 0x95 0xA5 0xB5 x
genCode (BITB x)    = addrImm8 0xC5 0xD5 0xE5 0xF5 x
genCode (BLE r)     = byte 0x2F >> rel8 r
genCode (BLO r)     = byte 0x25 >> rel8 r
genCode (BLS r)     = byte 0x23 >> rel8 r
genCode (BLT r)     = byte 0x2D >> rel8 r
genCode (BMI r)     = byte 0x2B >> rel8 r
genCode (BNE r)     = byte 0x26 >> rel8 r
genCode (BPL r)     = byte 0x2A >> rel8 r
genCode (BRA r)     = byte 0x20 >> rel8 r
genCode (BRN r)     = byte 0x21 >> rel8 r
genCode (BSR r)     = byte 0x8D >> rel8 r
genCode (BVC r)     = byte 0x28 >> rel8 r
genCode (BVS r)     = byte 0x29 >> rel8 r
genCode CLRA        = byte 0x4F
genCode CLRB        = byte 0x5F
genCode (CLR a)     = addr 0x0F 0x6F 0x7F a
genCode (CMPA x)    = addrImm8 0x81 0x91 0xA1 0xB1 x
genCode (CMPB x)    = addrImm8 0xC1 0xD1 0xE1 0xF1 x
genCode (CMPD x)    = byte 0x10 >> addrImm16 0x83 0x93 0xA3 0xB3 x
genCode (CMPS x)    = byte 0x11 >> addrImm16 0x8C 0x9C 0xAC 0xBC x
genCode (CMPU x)    = byte 0x11 >> addrImm16 0x83 0x93 0xA3 0xB3 x
genCode (CMPX x)    = addrImm16 0x8C 0x9C 0xAC 0xBC x
genCode (CMPY x)    = byte 0x10 >> addrImm16 0x8C 0x9C 0xAC 0xBC x
genCode COMA        = byte 0x43
genCode COMB        = byte 0x53
genCode (COM a)     = addr 0x03 0x63 0x73 a
genCode (CWAI n)    = byte 0x3C >> byte n
genCode DAA         = byte 0x19
genCode DECA        = byte 0x4A
genCode DECB        = byte 0x5A
genCode (DEC a)     = addr 0x0A 0x6A 0x7A a
genCode (EORA x)    = addrImm8 0x88 0x98 0xA8 0xB8 x
genCode (EORB x)    = addrImm8 0xC8 0xD8 0xE8 0xF8 x
genCode (EXG r0 r1) = byte 0x1E >> byte (reg2Byte r0 r1)
genCode INCA        = byte 0x4C
genCode INCB        = byte 0x5C
genCode (INC a)     = addr 0x0C 0x6C 0x7C a
genCode (JMP  a)    = addr 0x0E 0x6E 0x7E a
genCode (JSR  a)    = addr 0x9D 0xAD 0xBD a
genCode (LBCC r)    = word 0x1024 >> rel16 r
genCode (LBCS r)    = word 0x1025 >> rel16 r
genCode (LBEQ r)    = word 0x1027 >> rel16 r
genCode (LBGE r)    = word 0x102C >> rel16 r
genCode (LBGT r)    = word 0x102E >> rel16 r
genCode (LBHI r)    = word 0x1022 >> rel16 r
genCode (LBHS r)    = word 0x1024 >> rel16 r
genCode (LBLE r)    = word 0x102F >> rel16 r
genCode (LBLO r)    = word 0x1025 >> rel16 r
genCode (LBLS r)    = word 0x1023 >> rel16 r
genCode (LBLT r)    = word 0x102D >> rel16 r
genCode (LBMI r)    = word 0x102B >> rel16 r
genCode (LBNE r)    = word 0x1026 >> rel16 r
genCode (LBPL r)    = word 0x102A >> rel16 r
genCode (LBRA r)    = byte 0x16 >> rel16 r
genCode (LBRN r)    = word 0x1021 >> rel16 r
genCode (LBSR r)    = byte 0x17 >> rel16 r
genCode (LBVC r)    = word 0x1028 >> rel16 r
genCode (LBVS r)    = word 0x1029 >> rel16 r
genCode (LDA x)     = addrImm8  0x86 0x96 0xA6 0xB6 x
genCode (LDB x)     = addrImm8  0xC6 0xD6 0xE6 0xF6 x
genCode (LDD x)     = addrImm16 0xCC 0xDC 0xEC 0xFC x
genCode (LDS x)     = byte 0x10 >> addrImm16 0xCE 0xDE 0xEE 0xFE x
genCode (LDU x)     = addrImm16 0xCE 0xDE 0xEE 0xFE x
genCode (LDX x)     = addrImm16 0x8E 0x9E 0xAE 0xBE x
genCode (LDY x)     = byte 0x10 >> addrImm16 0x8E 0x9E 0xAE 0xBE x
genCode (LEAS a)    = byte 0x32 >> indexedAddress a
genCode (LEAU a)    = byte 0x33 >> indexedAddress a
genCode (LEAX a)    = byte 0x30 >> indexedAddress a
genCode (LEAY a)    = byte 0x31 >> indexedAddress a
genCode LSLA        = byte 0x48
genCode LSLB        = byte 0x58
genCode (LSL a)     = addr 0x08 0x68 0x78 a
genCode LSRA        = byte 0x44
genCode LSRB        = byte 0x54
genCode (LSR a)     = addr 0x04 0x64 0x74 a
genCode MUL         = byte 0x3D
genCode NEGA        = byte 0x40
genCode NEGB        = byte 0x50
genCode (NEG a)     = addr 0x00 0x60 0x70 a
genCode NOP         = byte 0x12
genCode (ORA x)     = addrImm8  0x8A 0x9A 0xAA 0xBA x
genCode (ORB x)     = addrImm8  0xCA 0xDA 0xEA 0xFA x
genCode (ORCC n)    = byte 0x1A >> byte n
genCode (PSHS rs)   =
  byte 0x34 >> byte (foldl (.|.) 0 $ map registerStackMask rs)
genCode (PSHU rs)   =
  byte 0x36 >> byte (foldl (.|.) 0 $ map registerStackMask rs)
genCode (PULS rs)   =
  byte 0x35 >> byte (foldl (.|.) 0 $ map registerStackMask rs)
genCode (PULU rs)   =
  byte 0x37 >> byte (foldl (.|.) 0 $ map registerStackMask rs)
genCode ROLA        = byte 0x49
genCode ROLB        = byte 0x59
genCode (ROL a)     = addr 0x09 0x69 0x79 a
genCode RORA        = byte 0x46
genCode RORB        = byte 0x56
genCode (ROR a)     = addr 0x06 0x66 0x76 a
genCode RTI         = byte 0x3B
genCode RTS         = byte 0x39
genCode (SBCA x)    = addrImm8  0x82 0x92 0xA2 0xB2 x
genCode (SBCB x)    = addrImm8  0xC2 0xD2 0xE2 0xF2 x
genCode SEX         = byte 0x1D
genCode (STA a)     = addr 0x97 0xA7 0xB7 a
genCode (STB a)     = addr 0xD7 0xE7 0xF7 a
genCode (STD a)     = addr 0xDD 0xED 0xFD a
genCode (STS a)     = byte 0x10 >> addr 0xDF 0xEF 0xFF a
genCode (STU a)     = addr 0xDF 0xEF 0xFF a
genCode (STX a)     = addr 0x9F 0xAF 0xBF a
genCode (STY a)     = byte 0x10 >> addr 0x9F 0xAF 0xBF a
genCode (SUBA x)    = addrImm8  0x80 0x90 0xA0 0xB0 x
genCode (SUBB x)    = addrImm8  0xC0 0xD0 0xE0 0xF0 x
genCode (SUBD x)    = addrImm16 0x83 0x93 0xA3 0xB3 x
genCode SWI         = byte 0x3F
genCode SWI2        = word 0x103F
genCode SWI3        = word 0x113F
genCode SYNC        = byte 0x13
genCode (TFR r0 r1) = byte 0x1F >> byte (reg2Byte r0 r1)
genCode TSTA        = byte 0x4D
genCode TSTB        = byte 0x5D
genCode (TST a)     = addr 0x0D 0x6D 0x7D a

regNibble :: Register -> Word8
regNibble RegD  = 0
regNibble RegX  = 1
regNibble RegY  = 2
regNibble RegU  = 3
regNibble RegS  = 4
regNibble RegPC = 5
regNibble RegA  = 8
regNibble RegB  = 9
regNibble RegCC = 10
regNibble RegDP = 11

registerStackMask :: Register -> Word8
registerStackMask RegD  = 0x06
registerStackMask RegX  = 0x10
registerStackMask RegY  = 0x20
registerStackMask RegU  = 0x40
registerStackMask RegS  = 0x40
registerStackMask RegPC = 0x80
registerStackMask RegA  = 0x02
registerStackMask RegB  = 0x04
registerStackMask RegCC = 0x01
registerStackMask RegDP = 0x08

indexRegister :: IndexRegister -> Word8
indexRegister IxX = 0
indexRegister IxY = 1
indexRegister IxU = 2
indexRegister IxS = 3

reg2Byte :: Register -> Register -> Word8
reg2Byte r0 r1 =
  (regNibble r0 `shiftL` 4) .|. regNibble r1

indexedAddress :: IndexedAddr -> ST.State AssemblerState ()
indexedAddress (IA_Index n r) =
  if n == 0 then byte (0x84 .|. (indexRegister r `shiftL` 5))
  else if n >= -16 && n <= 15 then
    byte ((fromIntegral n .&. 0x1F) .|. (indexRegister r `shiftL` 5))
  else if n >= -128 && n <= 127 then do
    byte (0x88 .|. (indexRegister r `shiftL` 5))
    byte (fromIntegral n)
  else do
    byte (0x89 .|. (indexRegister r `shiftL` 5))
    word (fromIntegral n)
indexedAddress (IA_IndexA r) = byte (0x86 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_IndexB r) = byte (0x85 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_IndexD r) = byte (0x8B .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_Incr1 r)  = byte (0x80 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_Incr2 r)  = byte (0x81 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_Decr1 r)  = byte (0x82 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_Decr2 r)  = byte (0x83 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_Label n l) = do
  curPos <- getPos
  lblPos <- getLabel l
  byte 0x8D
  word$ fromIntegral (lblPos + fromIntegral n - curPos - 3)
indexedAddress (IA_PCOff n) =
  if n >= -128 && n <= 127 then do
    byte 0x8C
    byte (fromIntegral n)
  else do
    byte 0x8D
    word (fromIntegral n)
indexedAddress (IA_IndirIndex n r) =
  if n == 0 then byte (0x94 .|. (indexRegister r `shiftL` 5))
  else if n >= -128 && n <= 127 then do
    byte (0x98 .|. (indexRegister r `shiftL` 5))
    byte (fromIntegral n)
  else do
    byte (0x99 .|. (indexRegister r `shiftL` 5))
    word (fromIntegral n)
indexedAddress (IA_IndirIndexA r) =
  byte (0x96 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_IndirIndexB r) =
  byte (0x95 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_IndirIndexD r) =
  byte (0x9B .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_IndirIncr2 r) = byte (0x91 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_IndirDecr2 r) = byte (0x93 .|. (indexRegister r `shiftL` 5))
indexedAddress (IA_IndirLabel n l) = do
  curPos <- getPos
  lblPos <- getLabel l
  byte 0x9D
  word$ fromIntegral (lblPos + fromIntegral n - curPos - 3)
indexedAddress (IA_IndirPCOff n) =
  if n >= -128 && n <= 127 then do
    byte 0x9C
    byte (fromIntegral n)
  else do
    byte 0x9D
    word (fromIntegral n)
indexedAddress (IA_IndirExtended n) = do
  byte 0x9F
  word (fromIntegral n)

addrImm8 :: Word8 -> Word8 -> Word8 -> Word8 -> AddrImm8 ->
  ST.State AssemblerState ()
addrImm8 op _ _ _ (Imm8 b) = byte op >> byte b
addrImm8 _ opd opi ope (Addr8 a) = addr opd opi ope a

addrImm16 :: Word8 -> Word8 -> Word8 -> Word8 -> AddrImm16 ->
  ST.State AssemblerState ()
addrImm16 op _ _ _ (Imm16 w) = byte op >> word w
addrImm16 _ opd opi ope (Addr16 a) = addr opd opi ope a

addr :: Word8 -> Word8 -> Word8 -> Addr -> ST.State AssemblerState ()
addr opd _ _ (ADirect n) = byte opd >> byte n
addr _ opi _ (AIndexed a) = byte opi >> indexedAddress a
addr _ _ ope (AExtended n) = byte ope >> word n

rel8 :: Rel8 -> ST.State AssemblerState ()
rel8 (R8Label l) = do
  curPos <- getPos
  lblPos <- getLabel l
  byte$ fromIntegral (lblPos - curPos - 1)
rel8 (R8Imm n) = byte$ fromIntegral n

rel16 :: Rel16 -> ST.State AssemblerState ()
rel16 (R16Label l) = do
  curPos <- getPos
  lblPos <- getLabel l
  word$ fromIntegral (lblPos - curPos - 2)
rel16 (R16Imm n) = word$ fromIntegral n

-- ============================================================================

setLabel :: Label -> ST.State AssemblerState ()
setLabel l =
  ST.modify' $ \ st -> st { asLabels = Map.insert l (asPos st) (asLabels st) }

getPos :: ST.State AssemblerState Int
getPos = ST.gets asPos

getLabel :: Label -> ST.State AssemblerState Int
getLabel l = do
  lmap <- ST.gets asLabelMap
  return$ lmap Map.! l

byte :: Word8 -> ST.State AssemblerState ()
byte b = do
  ST.modify' $ \ st ->
    st { asBytes = BS.word8 b : asBytes st, asPos = asPos st + 1 }

word :: Word16 -> ST.State AssemblerState ()
word w = do
  ST.modify' $ \ st ->
    st { asBytes = BS.word16BE w : asBytes st, asPos = asPos st + 2 }
