import AST
import qualified Parser
import qualified DataTypes
import qualified MacroAsm
import qualified MPU6809
import qualified Platform
import qualified Assembler


func1 =
 case Parser.parseProgram "ala" "3 div ((2+2)*7)" of
   Left err -> Left err
   Right (AST.Program fs vars body) -> Right (ev fs vars body)


data Val = VNum Integer
         | VBool Bool
         | VList Val Val
         | VNil
         | VPair Val Val
         | VOp String
           deriving (Eq, Show)


ev funcDef _ (ENum op n) = Just [VNum n]
ev funcDef gem (EBinary p op e1 e2) =
 case (op, w1, w2) of
  (BAdd, Just n1, Just n2) -> Just ( n1 ++ n2 ++ [VOp "+"] )
  (BSub, Just n1, Just n2) -> Just ( n1 ++ n2 ++ [VOp "+"] )
  (BMul, Just n1, Just n2) -> Just ( n1 ++ n2 ++ [VOp "*"] )
  (BDiv, Just n1, Just n2) -> Just ( n1 ++ n2 ++ [VOp "/"] )
  (_, _, _) -> Nothing
 where
  w1 = ev funcDef gem e1
  w2 = ev funcDef gem e2
