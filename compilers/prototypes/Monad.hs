import MacroAsm
import Control.Monad (liftM, ap)

data LabelDef = LabelDef
  { current      :: Int        --aktualne id etykiety
  }
  deriving (Show, Eq)

--sztuczny i niepotrzebny typ "a"
newtype Comp a = Comp
  { runComp :: LabelDef -> (LabelDef, [MInstr], a)
  }
  --Jeśli zastąpimy typ MInstr typem "a" nie będziemy mogli w >>= wykonać ++
  --z powodu niezgodności typów (argumentami będzie typ a i b)
  --Jeśli zastąpimy typ LabelDef typem "a" będziemy mieli niezgodność typów
  --(>>=) :: Monad m => m a -> (a -> m b) -> m b
  --my będziamy próbować wywaołać typ b na pierwszym argumencie, który ma typ m a
  --Jeśli pozbędziemy się zmiennej "a" w newtype będziemy mieli niezgodność typów
  --zamiast Monad * -> * będzie *

--niepotrzebne definicje
instance Functor Comp where
  fmap  = liftM

--więcej niepotrzebnych definicji
instance Applicative Comp where
  pure x  = Comp(\ s -> (s,[],x)) -- definicja jak dla return
  (<*>) = ap

--jeszcze więcej niepotrzebnych definicji
instance Monad Comp where
  return = pure
  fail _ = Comp(\ s -> (s,[],undefined)) -- definicja jak dla return
  --przydatne:
  Comp p >>= f = Comp(
    \label0 ->
    case p label0 of
      (label1, instr1, a1) -> case runComp (f a1) label1 of
                              (label2, instr2, a2) -> (label2, instr1 ++ instr2, a2)
    )
