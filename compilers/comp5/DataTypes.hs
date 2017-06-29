-- Moduł z typami potrzebnymi w zadaniu
{-# LANGUAGE Safe #-}
module DataTypes where

-- Treść komunikatu o błędzie
type ErrorMessage = String

-- Wynik spradzania typów
data TypeCheckResult p
  = Ok                   -- Program jest poprawny
  | Error p ErrorMessage -- Błąd na pozycji p o treści ErrorMessage
  deriving (Eq, Show)

-- Wynik obliczania programu
data EvalResult
  = Value Integer -- Poprawny wynik obliczeń
  | RuntimeError  -- Błąd podczas wykonania programu (dzielenie prze zero)
  deriving (Eq, Show)

-- Źródło programu dla testu
data ProgramSource
  = SrcString String   -- Bezpośrednia treść programu
  | SrcFile   FilePath -- ścieżka do pliku z programem
  deriving (Eq, Show)

-- Oczekiwana odpowiedź dla testu
data TestAnswer
  -- Niepoprawny program (błąd typu)
  = TypeError
  -- Eval params result -- Program jest poprawny, a dla wartości zmiennych
  -- wejściowych params obliczenie daje wynik result
  | Eval [Integer] EvalResult
  deriving (Eq, Show)

-- Test
data Test = Test
  { testName    :: String        -- Nazwa testu
  , testProgram :: ProgramSource -- Źródło porogramu
  , testAnswer  :: TestAnswer    -- Oczekiwany rezultat
  }
  deriving (Eq, Show)
