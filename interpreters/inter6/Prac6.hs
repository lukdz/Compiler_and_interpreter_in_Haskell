-- Sprawdzaczka do zadania 6
--
-- Sprawdzaczka pozwala zarówno sprawdzić swoje rozwiązanie na własnych
-- testach, jak i uruchomić napisany ewaluator na dowolnym programie
--
-- Aby uruchomić swoje rozwiązanie na testach należy uruchomić program
-- z flagą -t:
--
-- $ ./Prac6 -t
--
-- Aby uruchomić ewaluator na przygotowanym pliku z programem, przekazujemy
-- ścieżkę do pliku (np. example.pp6) do sprawdzaczki:
--
-- $ ./Prac6 example.pp6
--
-- Dodatkowo flagi -u oraz -d pozwalają wyłączyć sprawdzanie typów
-- i ewaluacje programu. Zatem można
--
-- * uruchomić program bez sprawdzania typów:
--   $ ./Prac6 -u example.pp6
-- * sprawdzić typy bez uruchamiania programu:
--   $ ./Prac6 -d example.pp6
-- * tylko sparsować program:
--   $ ./Prac6 -u -d example.pp6
{-# LANGUAGE Safe #-}

-- Należy zmodyfikować poniższe dwa wiersze zmieniając nazwy modułów
--import qualified ImieNazwiskoTests as Tests
import qualified LukaszDzwoniarekTests as Tests
--import qualified ImieNazwisko      as Solution
import qualified LukaszDzwoniarek      as Solution

import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Error
import Text.Parsec.Pos
import Data.List
import Control.Exception

import qualified AST
import qualified Parser
import qualified DataTypes

data Flag = Test | NoTypecheck | NoEval | Help deriving (Eq)

options =
  [ Option ['t'] ["test"]         (NoArg Test)
      "run all tests"
  , Option ['u'] ["no-typecheck"] (NoArg NoTypecheck)
      "do not typecheck programs"
  , Option ['d'] ["no-eval"]      (NoArg NoEval)
      "do not run programs"
  , Option ['h'] ["help"] (NoArg Help) "display this list of options"
  ]

usageHeader = "Usage: Prac6 [OPTION...] files..."

data Options = Options
  { noTypecheck :: Bool
  , noEval      :: Bool
  }

data Action
  = AMessage String
  | ATestRun Options [SourceName]
  | ARun     Options [SourceName]

parseArgs :: [String] -> Action
parseArgs args =
  case getOpt RequireOrder options args of
    (flags,fnames,[]) ->
      if elem Help flags then
        AMessage $ usageInfo usageHeader options
      else if elem Test flags then
        ATestRun (opts flags) fnames
      else
        ARun (opts flags) fnames
    (_,_, errs) ->
        AMessage $ concat errs ++ usageInfo usageHeader options
  where
    opts flags = Options
      { noTypecheck = elem NoTypecheck flags
      , noEval      = elem NoEval      flags
      }

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    AMessage msg -> hPutStrLn stderr msg
    ATestRun opts fnames -> do
      runAllTests
      runPrograms opts fnames
    ARun opts fnames -> runPrograms opts fnames

parseProgram :: SourceName -> String ->
    IO ([AST.FunctionDef SourcePos], [AST.Var], AST.Expr SourcePos)
parseProgram fname chars =
  case Parser.parseProgram fname chars of
    Right (AST.Program fs vars body) -> do
      checkGlobalUniqueness (funcVars fs ++ vars)
      return (fs, map fst vars, body)
    Left error -> ioError $ userError $ show error
  where
    funcVars = map$ \ fd -> (AST.funcName fd, AST.funcPos fd)

parseProgramFile :: SourceName ->
  IO ([AST.FunctionDef SourcePos], [AST.Var], AST.Expr SourcePos)
parseProgramFile fname = do
  chars <- readFile fname
  parseProgram fname chars

checkGlobalUniqueness :: [(AST.Var, SourcePos)] -> IO ()
checkGlobalUniqueness [] = return ()
checkGlobalUniqueness ((x, p) : xs) =
  case find (\ (y, _) -> x == y) xs of
    Nothing -> checkGlobalUniqueness xs
    Just _  ->
      ioError $ userError $ show p ++
        (":\nRedefinition of global variable " ++ x)

runAllTests :: IO ()
runAllTests = mapM_ runTest Tests.tests

runTest :: DataTypes.Test -> IO ()
runTest (DataTypes.Test name src ans) = do
  printTestName
  (fs, vars, body) <- loadTestProgram src
  case (ans, Solution.typecheck fs vars body) of
    (DataTypes.TypeError, DataTypes.Ok) ->
      putStrLn "wrong type-checker answer"
    (DataTypes.TypeError, DataTypes.Error _ _) ->
      putStrLn "ok"
    (DataTypes.Eval _ _, DataTypes.Error _ _) ->
      putStrLn "wrong type-checker answer"
    (DataTypes.Eval input res, DataTypes.Ok) ->
      if res == Solution.eval fs (zip vars input) body then
        putStrLn "ok"
      else
        putStrLn "wrong answer"
  where
    printTestName = do
      putStr $ name ++ replicate (16 - length name) ' ' ++ ": "
      hFlush stdout
    loadTestProgram (DataTypes.SrcString str) =
      parseProgram name str
    loadTestProgram (DataTypes.SrcFile fname) =
      parseProgramFile fname

runPrograms :: Options -> [SourceName] -> IO ()
runPrograms opts = mapM_ $ runProgram opts

runProgram :: Options -> SourceName -> IO ()
runProgram opts fname =
  catches (do
    (fs, vars, body) <- parseProgramFile fname
    if noTypecheck opts then return () else typecheckProgram fs vars body
    if noEval      opts then return () else evalProgram fs vars body
  )
  [ Handler (hPutStrLn stderr . ioeGetErrorString)
  , Handler (\ e -> hPutStrLn stderr $ show (e :: SomeException))
  ]

typecheckProgram ::
  [AST.FunctionDef SourcePos] -> [AST.Var] -> AST.Expr SourcePos -> IO ()
typecheckProgram fs vars body =
  case Solution.typecheck fs vars body of
    DataTypes.Ok            -> return ()
    DataTypes.Error pos msg ->
      ioError $ userError $ show pos ++ ":\n" ++ msg

evalProgram ::
  [AST.FunctionDef SourcePos] -> [AST.Var] -> AST.Expr SourcePos -> IO ()
evalProgram fs vars body = do
  input <- mapM readInput vars
  putStrLn $ show $ Solution.eval fs input body
  where
    readInput :: AST.Var -> IO (AST.Var, Integer)
    readInput x = do
      putStr (x ++ " = ")
      hFlush stdout
      v <- readLn
      return $ (x, v)
