
-- stack ghci --package silently --package containers --package HUnit interpretador-25-04-26.hs

module Interp where

import Data.Map (Map)
import qualified Data.Map as Map
import Test.HUnit
import System.IO.Silently (capture_)
import Test.HUnit (Test(TestLabel))

-- Map é usado para armazenar os identificadores do programa
-- Funcões utilizadas: Map.empty, Map.insert e Map.lookup

-- ============================================================
-- Tipos da arvore sintatica 
-- ============================================================

data Type
  = TInt
  | TBool
  | TVoid
  | TFloat
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq  | Neq | Lt  | Gt  | Le | Ge
  | And | Or
  deriving (Show, Eq)

data Expr
  = Lit   Int
  | FLit  Double
  | BLit  Bool
  | Var   String
  | BinE  BinOp Expr Expr
  | Not   Expr
  | Call  String [Expr]
  deriving (Show, Eq)

data Stmt
  = Decl   Type String Expr    -- int x = 10; 
  | Assign String Expr         -- x = 20 + 10
  | If     Expr Stmt Stmt
  | While  Expr Stmt
  | Block  [Stmt]
  | Return Expr                -- return x + y
  | Print  Expr                -- printf (10)
  | Skip
  deriving (Show, Eq)

data FunDecl = FunDecl
  { funName   :: String
  , funParams :: [(Type, String)]-- [(TInt, "x"), (TInt,"y")]
  , funBody   :: Stmt
  } deriving (Show, Eq)

-- ============================================================
-- Valores e estado do programa (ambiente)
-- ============================================================

-- programas lidam com inteiros e booleanos
data Value
  = VInt  Int
  | VBool Bool
  | VFloat Double
  | VVoid
  deriving (Show, Eq)

-- variaveis no escopo do programa
type Env    = Map String Value

-- funções no escopo do programa
type FunEnv = Map String FunDecl

-- ============================================================
-- Resultado da execução de um Stmt
-- ============================================================

data StmtResult
  = Next     Env
  | Returned Value Env
  deriving (Show)

-- ============================================================
-- Avaliação de Expressões 
-- ============================================================

evalExpr :: FunEnv -> Env -> Expr -> IO Value
evalExpr fenv env expr = case expr of

  Lit  n -> return (VInt n)
  BLit b -> return (VBool b)
  FLit f -> return (VFloat f)

  Var x  -> case Map.lookup x env of
               Nothing -> error ("Variavel nao declarada: " ++ x)
               Just v  -> return v

  Not e  -> do
    v <- evalExpr fenv env e
    case v of
      VBool b -> return (VBool (not b))
      _       -> error "Not aplicado a nao-booleano"

  BinE op l r -> do
    vl <- evalExpr fenv env l
    vr <- evalExpr fenv env r
    return (evalBinOp op vl vr)

  Call fname args ->
    case Map.lookup fname fenv of
      Nothing  -> error ("Funcao nao declarada: " ++ fname)
      Just fun -> do
        let nparams = length (funParams fun)
            nargs   = length args
        if nparams /= nargs
          then error ("Funcao " ++ fname ++ " espera " ++
                      show nparams ++ " argumentos, recebeu " ++
                      show nargs)
          else do
            vals <- mapM (evalExpr fenv env) args
            let params   = map snd (funParams fun)
                envLocal = Map.fromList (zip params vals)
            result <- execStmt fenv envLocal (funBody fun)
            case result of
              Returned v _ -> return v
              Next _       -> return VVoid

-- ============================================================
-- evalBinOp — permanece pura
-- ============================================================

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Add (VInt   a) (VInt   b) = VInt   (a + b)
evalBinOp Add (VFloat a) (VFloat b) = VFloat (a + b)
evalBinOp Sub (VInt  a) (VInt  b) = VInt  (a - b)
evalBinOp Mul (VInt  a) (VInt  b) = VInt  (a * b)
evalBinOp Div (VInt  a) (VInt  b) = VInt  (a `div` b)
evalBinOp Mod (VInt  a) (VInt  b) = VInt  (a `mod` b)
evalBinOp Eq  va        vb        = VBool (va == vb)
evalBinOp Neq va        vb        = VBool (va /= vb)
evalBinOp Lt  (VInt  a) (VInt  b) = VBool (a <  b)
evalBinOp Gt  (VInt  a) (VInt  b) = VBool (a >  b)
evalBinOp Le  (VInt  a) (VInt  b) = VBool (a <= b)
evalBinOp Ge  (VInt  a) (VInt  b) = VBool (a >= b)
evalBinOp And (VBool a) (VBool b) = VBool (a && b)
evalBinOp Or  (VBool a) (VBool b) = VBool (a || b)
evalBinOp op  _         _         =
  error ("Operacao invalida ou tipos incompativeis: " ++ show op)

-- ============================================================
-- Execução de Statements
-- ============================================================

execStmt :: FunEnv -> Env -> Stmt -> IO StmtResult
execStmt fenv env stmt = case stmt of

  Skip ->
    return (Next env)

  Print e -> do
    v <- evalExpr fenv env e
    putStrLn (showValue v)
    return (Next env)

  Decl _ x e -> do
    v <- evalExpr fenv env e
    let env' = Map.insert x v env
    return (Next env')

  Assign x e ->
    case Map.lookup x env of
      Nothing -> error ("Variavel nao declarada: " ++ x)
      Just _  -> do
        v <- evalExpr fenv env e
        let env' = Map.insert x v env
        return (Next env')

  If cond t f -> do
    v <- evalExpr fenv env cond
    case v of
      VBool True  -> execStmt fenv env t
      VBool False -> execStmt fenv env f
      _           -> error "Condicao do If nao eh booleana"

  Return e -> do
    v <- evalExpr fenv env e
    return (Returned v env)

  Block stmts ->
    execBlock fenv env stmts

  While cond body -> do
    v <- evalExpr fenv env cond
    case v of
      VBool False -> return (Next env)
      VBool True  -> do
        r <- execStmt fenv env body
        case r of
          Returned v' env' -> return (Returned v' env')
          Next        env' -> execStmt fenv env' (While cond body)
      _ -> error "Condicao do While nao eh booleana"

-- ============================================================
-- Execução de um bloco
-- ============================================================

execBlock :: FunEnv -> Env -> [Stmt] -> IO StmtResult
execBlock _    env []     = return (Next env)
execBlock fenv env (s:ss) = do
  r <- execStmt fenv env s
  case r of
    Returned v env' -> return (Returned v env')
    Next       env' -> execBlock fenv env' ss

-- ============================================================
-- Utilitários
-- ============================================================

showValue :: Value -> String
showValue (VInt  n) = show n
showValue (VFloat f) = show f
showValue (VBool b) = if b then "verdadeiro" else "falso"
showValue VVoid     = "()"

-- ============================================================
-- Auxiliares para testes
-- ============================================================

-- Executa um programa e extrai o valor retornado via Return
runProg :: FunEnv -> Stmt -> IO Value
runProg fenv prog = do
  result <- execStmt fenv Map.empty prog
  case result of
    Returned v _ -> return v
    Next _       -> return VVoid

-- Executa um programa e captura tudo que foi impresso via Print
runProgIO :: FunEnv -> Stmt -> IO String
runProgIO fenv prog =
  capture_ (execStmt fenv Map.empty prog)

-- ============================================================
-- Funções de exemplo
-- ============================================================

-- int dobro(int x) { retorna x * 2; }
funDobro :: FunDecl
funDobro = FunDecl
  { funName   = "dobro"
  , funParams = [(TInt, "x")]
  , funBody   = Return (BinE Mul (Var "x") (Lit 2))
  }

-- int fatorial(int n) {
--   se (n == 0) retorna 1
--   senao retorna n * fatorial(n - 1)
-- }
funFatorial :: FunDecl
funFatorial = FunDecl
  { funName   = "fatorial"
  , funParams = [(TInt, "n")]
  , funBody   =
      If (BinE Eq (Var "n") (Lit 0))
         (Return (Lit 1))
         (Return (BinE Mul
                    (Var "n")
                    (Call "fatorial" [BinE Sub (Var "n") (Lit 1)])))
  }

-- int soma(int a, int b) { retorna a + b; }
funSoma :: FunDecl
funSoma = FunDecl
  { funName   = "soma"
  , funParams = [(TInt, "a"), (TInt, "b")]
  , funBody   = Return (BinE Add (Var "a") (Var "b"))
  }

fenvExemplo :: FunEnv
fenvExemplo = Map.fromList
  [ ("dobro",    funDobro)
  , ("fatorial", funFatorial)
  , ("soma",     funSoma)
  ]

-- ============================================================
-- Programas de teste
-- ============================================================

-- int x = 10; int y = 3; retorna x + y;
prog1 :: Stmt
prog1 = Block
  [ Decl TInt "x" (Lit 10)
  , Decl TInt "y" (Lit 3)
  , Return (BinE Add (Var "x") (Var "y"))
  ]

-- int n = 7; se (n > 5) retorna verdadeiro senao retorna falso
prog2 :: Stmt
prog2 = Block
  [ Decl TInt "n" (Lit 7)
  , If (BinE Gt (Var "n") (Lit 5))
       (Return (BLit True))
       (Return (BLit False))
  ]

-- int i = 0; enquanto (i < 3) { i = i + 1; } retorna i;
prog3 :: Stmt
prog3 = Block
  [ Decl TInt "i" (Lit 0)
  , While (BinE Lt (Var "i") (Lit 3))
      (Assign "i" (BinE Add (Var "i") (Lit 1)))
  , Return (Var "i")
  ]

-- retorna dobro(5)
prog4 :: Stmt
prog4 = Return (Call "dobro" [Lit 5])

-- retorna fatorial(5)
prog5 :: Stmt
prog5 = Return (Call "fatorial" [Lit 5])

-- int x = 4; retorna soma(dobro(x), 3)
prog6 :: Stmt
prog6 = Block
  [ Decl TInt "x" (Lit 4)
  , Return (Call "soma" [Call "dobro" [Var "x"], Lit 3])
  ]

-- int i = 0; enquanto (i < 3) { imprime(i); i = i + 1; }
prog7 :: Stmt
prog7 = Block
  [ Decl TInt "i" (Lit 0)
  , While (BinE Lt (Var "i") (Lit 3))
      (Block
        [ Print (Var "i")
        , Assign "i" (BinE Add (Var "i") (Lit 1))
        ])
  ]

-- imprime(dobro(4))
prog8 :: Stmt
prog8 = Print (Call "dobro" [Lit 4])

-- ============================================================
-- Suite de testes HUnit
-- ============================================================

-- testes de valor retornado (usam runProg)

testSomaSimples :: Test
testSomaSimples = TestCase $ do
  v <- runProg Map.empty prog1
  assertEqual "soma 10 + 3" (VInt 13) v

testCondicionalVerdade :: Test
testCondicionalVerdade = TestCase $ do
  v <- runProg Map.empty prog2
  assertEqual "7 > 5 eh verdadeiro" (VBool True) v

testWhileContaAte3 :: Test
testWhileContaAte3 = TestCase $ do
  v <- runProg Map.empty prog3
  assertEqual "while incrementa i ate 3" (VInt 3) v

testDobro :: Test
testDobro = TestCase $ do
  v <- runProg fenvExemplo prog4
  assertEqual "dobro de 5 eh 10" (VInt 10) v

testFatorial :: Test
testFatorial = TestCase $ do
  v <- runProg fenvExemplo prog5
  assertEqual "fatorial de 5 eh 120" (VInt 120) v

testChamadaAninhada :: Test
testChamadaAninhada = TestCase $ do
  v <- runProg fenvExemplo prog6
  assertEqual "soma(dobro(4), 3) eh 11" (VInt 11) v

-- testes de saída impressa (usam runProgIO)

testWhileImprime :: Test
testWhileImprime = TestCase $ do
  output <- runProgIO Map.empty prog7
  assertEqual "while imprime 0, 1, 2" "0\n1\n2\n" output

testImprimeCallDobro :: Test
testImprimeCallDobro = TestCase $ do
  output <- runProgIO fenvExemplo prog8
  assertEqual "imprime dobro(4)" "8\n" output

testSomaFloatSimples :: Test
testSomaFloatSimples = TestCase $ do
  let resultado = evalBinOp Add (VFloat 2.5) (VFloat 1.5)
  assertEqual "soma de 2.5 + 1.5 deve ser 4.0" (VFloat 4.0) resultado

-- suite completa

suite :: Test
suite = TestList
  [ TestLabel "soma simples"         testSomaSimples
  , TestLabel "Soma Float Simples"   testSomaFloatSimples
  , TestLabel "condicional"          testCondicionalVerdade
  , TestLabel "while retorna"        testWhileContaAte3
  , TestLabel "chamada dobro"        testDobro
  , TestLabel "fatorial recursivo"   testFatorial
  , TestLabel "chamada aninhada"     testChamadaAninhada
  , TestLabel "while imprime"        testWhileImprime
  , TestLabel "imprime call dobro"   testImprimeCallDobro
  ]

main :: IO ()
main = runTestTTAndExit suite