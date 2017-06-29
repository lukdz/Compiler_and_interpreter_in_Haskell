{-# LANGUAGE Safe #-}
{- Program pozwalający uruchamiać napisany kompilator
 - (wersja dla zadania 6)
 -
 - Należy zmodyfikować następne dwa wiersze
 - wstawiając nazwy modułów zawierających rozwiązanie
 - pracowni 6 oraz rozwiązanie zadania dodatkowego -}
import qualified ImieNazwisko         as Solution
import qualified ImieNazwiskoCompiler as Compiler

import System.Console.GetOpt
import System.Environment
import System.IO
import System.IO.Error
import Text.Parsec.Pos
import Data.List
import Control.Exception
import Control.Monad

import qualified AST
import qualified Parser
import qualified DataTypes
import qualified MacroAsm
import qualified MPU6809
import qualified Platform
import qualified Assembler

import qualified Data.ByteString.Lazy as BS
import System.FilePath.Posix (replaceExtension)

data Flag
  = NoTypecheck
  | TgtMacroasm | TgtAsm | RawOutput | TgtFile String
  | Help deriving (Eq)

data Options = Options
  { noTypecheck :: Bool
  , tgtMacroasm :: Bool
  , tgtAsm      :: Bool
  , rawOutput   :: Bool
  , tgtFile     :: Maybe FilePath
  }

data Action
  = AMessage String
  | ACompile Options SourceName

options =
  [ Option ['u'] ["no-typecheck"] (NoArg NoTypecheck)
      "do not typecheck programs"
  , Option ['m'] ["macroasm"] (NoArg TgtMacroasm)
      "compile to macroassembler"
  , Option ['s'] ["asm"] (NoArg TgtAsm)
      "compile to assembler"
  , Option ['o'] ["output"] (ReqArg TgtFile "FILE")
      "write output to FILE"
  , Option ['r'] ["raw"] (NoArg RawOutput)
      "do not generate prolog and epilog"
  , Option ['h'] ["help"] (NoArg Help) "display this list of options"
  ]

usageHeader = "Usage: Comp6 [OPTION...] file"

parseArgs :: [String] -> Action
parseArgs args =
  case getOpt RequireOrder options args of
    (flags,[fname],[]) ->
      if elem Help flags then
        AMessage $ usageInfo usageHeader options
      else
        ACompile (opts flags) fname
    (_,_, errs) ->
        AMessage $ concat errs ++ usageInfo usageHeader options
  where
    opts flags = Options
      { noTypecheck = elem NoTypecheck flags
      , tgtMacroasm = elem TgtMacroasm flags
      , tgtAsm      = elem TgtAsm flags
      , rawOutput   = elem RawOutput flags
      , tgtFile     = msum$ map tryTgtFile flags
      }
    tryTgtFile (TgtFile s) = Just s
    tryTgtFile _           = Nothing

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    AMessage msg -> hPutStrLn stderr msg
    ACompile opts fname -> compileProgram opts fname

parseProgram :: SourceName -> String ->
    IO ([AST.FunctionDef SourcePos], [AST.Var], AST.Expr SourcePos)
parseProgram fname chars =
  case Parser.parseProgram fname chars of
    Right (AST.Program fs vars body) -> do
      checkFunctionDefUniqueness fs
      checkInputUniqueness vars
      return (fs, map fst vars, body)
    Left error -> ioError $ userError $ show error

parseProgramFile :: SourceName ->
  IO ([AST.FunctionDef SourcePos], [AST.Var], AST.Expr SourcePos)
parseProgramFile fname = do
  chars <- readFile fname
  parseProgram fname chars

checkInputUniqueness :: [(AST.Var, SourcePos)] -> IO ()
checkInputUniqueness [] = return ()
checkInputUniqueness ((x, p) : xs) =
  case find (\ (y, _) -> x == y) xs of
    Nothing -> checkInputUniqueness xs
    Just _  ->
      ioError $ userError $ show p ++
        (":\nInput variable " ++ x ++ " is not unique")

checkFunctionDefUniqueness :: [AST.FunctionDef SourcePos] -> IO ()
checkFunctionDefUniqueness [] = return ()
checkFunctionDefUniqueness (AST.FunctionDef pos name _ _ _ _ : fs) =
  case find (\ f -> name == AST.funcName f) fs of
    Nothing -> checkFunctionDefUniqueness fs
    Just _  ->
      ioError $ userError $ show pos ++
        (":\nRedefinition of function " ++ name)

compileProgram :: Options -> SourceName -> IO ()
compileProgram opts fname =
  catches (do
    (fs, vars, body) <- parseProgramFile fname
    if noTypecheck opts then return () else typecheckProgram fs vars body
    let mcode = Compiler.compile fs vars body
    if tgtMacroasm opts then writeOutput opts (printMCode mcode)
    else if tgtAsm opts then writeOutput opts (printCode opts vars mcode)
    else writeBinaryOutput opts vars (binaryOutputFile fname opts) mcode
  ) 
  [ Handler (hPutStrLn stderr . ioeGetErrorString)
  , Handler (\ e -> hPutStrLn stderr $ show (e :: SomeException))
  ]

binaryOutputFile :: SourceName -> Options -> FilePath
binaryOutputFile fname opts =
  case tgtFile opts of
    Just f  -> f
    Nothing -> replaceExtension fname "b09"

typecheckProgram ::
  [AST.FunctionDef SourcePos] -> [AST.Var] -> AST.Expr SourcePos -> IO ()
typecheckProgram fs vars body =
  case Solution.typecheck fs vars body of
    DataTypes.Ok            -> return ()
    DataTypes.Error pos msg ->
      ioError $ userError $ show pos ++ ":\n" ++ msg

writeOutput :: Options -> (Handle -> IO ()) -> IO ()
writeOutput opts wr =
  case tgtFile opts of
    Nothing    -> wr stdout
    Just fname -> withFile fname WriteMode wr

writeBinaryOutput ::
  Options -> [AST.Var] -> FilePath -> [MacroAsm.MInstr] -> IO ()
writeBinaryOutput opts vars fname code =
  BS.writeFile fname $ Assembler.assembly $
    genProlog opts vars $ concatMap MacroAsm.unfoldMInstr code

printMCode :: [MacroAsm.MInstr] -> Handle -> IO ()
printMCode code h = mapM_ (hPutStrLn h . MacroAsm.prettyMInstr) code

printCode :: Options -> [AST.Var] -> [MacroAsm.MInstr] -> Handle -> IO ()
printCode opts vars code h =
  mapM_ (hPutStrLn h . MPU6809.prettyInstr) $
    genProlog opts vars $ concatMap MacroAsm.unfoldMInstr code

genProlog :: Options -> [AST.Var] -> [MPU6809.Instr] -> [MPU6809.Instr]
genProlog opts =
  if rawOutput opts then const id
  else Platform.preamble
