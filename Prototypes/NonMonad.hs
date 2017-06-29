import MacroAsm

data LabelDef = LabelDef
  { current      :: Int        --aktualne id etykiety
  }
  deriving (Show, Eq)

newtype Comp = Comp
  { runComp :: LabelDef -> (LabelDef, [MInstr])
  }

(@@) :: Comp -> Comp -> Comp
c1 @@ c2 =
  Comp (\ label0 -> let (label1, instr1) = runComp c1 label0 in
                    let (label2, instr2) = runComp c2 label1 in
                      (label2, instr1 ++ instr2)
       )
{-
--Dlaczego na where jest parse error?:
c1 @@ c2 =
  Comp (\label0 -> (label2, instr1 ++ instr2)
          where
            (label1, instr1) = runComp c1 label0
            (label2, instr2) = runComp c2 label1
       )
-}

{-
*Main> runComp  (func1 @@ func2 @@ func3) (LabelDef 1)
(LabelDef {current = 3},[MConst 1,MConst 2,MConst 3])
*Main> runComp  func4 (LabelDef 1)
(LabelDef {current = 3},[MConst 1,MConst 2,MConst 3])
-}

func1 :: Comp
func1 =
  Comp(\x -> (LabelDef 1, [MConst 1]))
func2 :: Comp
func2 =
  Comp(\x -> (LabelDef 2, [MConst 2]))
func3 :: Comp
func3 =
  Comp(\x -> (LabelDef 3, [MConst 3]))
func4 :: Comp
func4 =
  func1 @@ func2 @@ func3
