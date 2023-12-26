-- PFL 2023/24 - Haskell practical assignment quickstart
-- Additional imports required for the code
import Data.List (intercalate, sortBy, deleteBy)
import Data.Function (on)
import Data.Ord (comparing)
import qualified Text.Parsec as Parsec
import Text.Parsec.String ( Parser )

-- Part 1

-- Do not modify our definition of Inst and Code
data Inst =
    Push Integer | Add | Mult | Sub |
    Tru | Fals | Equ | Le | And | Neg |
    Fetch String | Store String | Noop |
    Branch Code Code | Loop Code Code
    deriving Show
type Code = [Inst]

-- Define a new type to represent the machine’s stack.
type Stack = [Either Integer Bool]

-- Define a new type to represent the machine’s state.
type State = [(String, Either Integer Bool)]

-- Implement the createEmptyStack function which returns an empty machine’s stack.
createEmptyStack :: Stack
createEmptyStack = []

-- Implement the stack2Str function which converts a stack given as input to a string.
stack2Str :: Stack -> String
stack2Str = intercalate "," . map showStackValue
  where
    showStackValue (Left i) = show i
    showStackValue (Right b) = if b then "True" else "False"

-- Implement the createEmptyState function which returns an empty machine’s state.
createEmptyState :: State
createEmptyState = []

-- Implement the state2Str function which converts a machine state given as input to a string.
state2Str :: State -> String
state2Str = intercalate "," . map showStatePair . sortBy (comparing fst)
  where
    showStatePair (var, Left i) = var ++ "=" ++ show i
    showStatePair (var, Right b) = var ++ "=" ++ (if b then "True" else "False")

-- Implement the run function.
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (inst:code, stack, state) = case inst of
    Push n -> run (code, Left n : stack, state)
    Add -> case stack of
        (Left i1 : Left i2 : rest) -> run (code, Left (i2 + i1) : rest, state)
        _ -> error "Run-time error"
    Mult -> case stack of
        (Left i1 : Left i2 : rest) -> run (code, Left (i2 * i1) : rest, state)
        _ -> error "Run-time error"
    Sub -> case stack of
        (Left i1 : Left i2 : rest) -> run (code, Left (i1 - i2) : rest, state)
        _ -> error "Run-time error"
    Tru -> run (code, Right True : stack, state)
    Fals -> run (code, Right False : stack, state)
    Equ -> case stack of
        (Left i1 : Left i2 : rest) -> run (code, Right (i1 == i2) : rest, state)
        (Right b1 : Right b2 : rest) -> run (code, Right (b1 == b2) : rest, state)
        _ -> error "Run-time error"
    Le -> case stack of
        (Left i1 : Left i2 : rest) -> run (code, Right (i1 <= i2) : rest, state)
        _ -> error "Run-time error"
    And -> case stack of
        (Right b1 : Right b2 : rest) -> run (code, Right (b1 && b2) : rest, state)
        _ -> error "Run-time error"
    Neg -> case stack of
        (Right b : rest) -> run (code, Right (not b) : rest, state)
        _ -> error "Run-time error"
    Fetch x -> case lookup x state of
        Just val -> run (code, val : stack, state)
        Nothing -> error "Run-time error"
    Store x -> case stack of
        (val : rest) -> run (code, rest, (x, val) : deleteBy ((==) `on` fst) (x, undefined) state)
        _ -> error "Run-time error"
    Noop -> run (code, stack, state)
    Branch c1 c2 -> case stack of
        (Right True : rest) -> run (c1 ++ code, rest, state)
        (Right False : rest) -> run (c2 ++ code, rest, state)
        _ -> error "Run-time error"
    Loop c1 c2 -> run (c1 ++ [Branch (c2 ++ [inst]) [Noop]], stack, state)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
    where (_, stack, state) = run (code, createEmptyStack, createEmptyState)

main :: IO ()
main = do
    print (testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10",""))
    print (testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True"))
    print (testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False"))
    print (testAssembler [Push (-20),Tru,Fals] == ("False,True,-20",""))
    print (testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20",""))
    print (testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20",""))
    print (testAssembler [Push (-20),Push (-21), Le] == ("True",""))
    print (testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4"))
    print (testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1"))
    -- let (stack, storage) = testAssembler  [Push 1,Push 2,And]
    -- putStrLn $ stack ++ storage
    -- If you test:
    -- testAssembler [Push 1,Push 2,And]
    -- You should get an exception with the string: "Run-time error"
    -- If you test:
    -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    -- You should get an exception with the string: "Run-time error"

    -- Run the program
    -- let (finalCode, finalStack, finalState) = run (program, initialStack, initialState)
    -- putStrLn $ "(\"" ++ stackResult ++ "\",\"" ++ stateResult ++ "\")"
    -- Testing compA
    putStrLn "Testing compA:"
    putStrLn $ "compA (Const 5) -> " ++ testCompA (Const 5)  -- Expected: [Push 5]
    putStrLn $ "compA (AddExp (Const 3) (Const 2)) -> " ++ testCompA (AddExp (Const 3) (Const 2))  -- Expected: [Push 2, Push 3, Add]

    -- Testing compB
    putStrLn "Testing compB:"
    putStrLn $ "compB TrueConst -> " ++ testCompB TrueConst  -- Expected: [Tru]
    putStrLn $ "compB (AndExp TrueConst FalseConst) -> " ++ testCompB (AndExp TrueConst FalseConst)  -- Expected: [Tru, Fals, And]

    -- Testing compile
    putStrLn "Testing compile:"
    putStrLn $ "compile [Assign \"x\" (Const 5)] -> " ++ testCompile [Assign "x" (Const 5)]
    -- [Push 5,Store "x"]

-- Function to test compA
testCompA :: Aexp -> String
testCompA expr = show (compA expr)

-- Function to test compB
testCompB :: Bexp -> String
testCompB expr = show (compB expr)

-- Function to test compile
testCompile :: [Stm] -> String
testCompile program = show (compile program)


-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- Tipos de Dados para Expressões e Instruções
data Aexp =
    Const Integer |
    Var String |
    AddExp Aexp Aexp |
    SubExp Aexp Aexp |
    MultExp Aexp Aexp
    deriving Show

data Bexp =
    TrueConst |
    FalseConst |
    Eq Aexp Aexp |
    LeExp Aexp Aexp |
    AndExp Bexp Bexp |
    Not Bexp
    deriving Show

data Stm =
    Assign String Aexp |
    Seq Stm Stm |
    If Bexp Stm Stm |
    While Bexp Stm
    deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA (Const n) = [Push n]
compA (Var x) = [Fetch x]
compA (AddExp a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SubExp a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MultExp a1 a2) = compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB TrueConst = [Tru]
compB FalseConst = [Fals]
compB (Eq a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (LeExp a1 a2) = compA a2 ++ compA a1 ++ [Le]
compB (AndExp b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (Not b) = compB b ++ [Neg]

compile :: Program -> Code
compile = concatMap compileStm
  where
    compileStm :: Stm -> Code
    compileStm (Assign x a) = compA a ++ [Store x]
    compileStm (Seq s1 s2) = compileStm s1 ++ compileStm s2
    compileStm (If b s1 s2) = compB b ++ [Branch (compileStm s1) (compileStm s2)]
    compileStm (While b s) = [Loop (compB b ++ [Neg]) (compileStm s)]

-- parse :: String -> Program
parse = undefined -- TODO
-- parse input = map parseStm $ splitOnSemicolon $ lexer input

--Hint: define an auxiliary function lexer :: String → [String] 
-- that splits the string into a list of words (tokens). 
--Example:
-- lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]
-- and from this list of tokens build the corresponding data, 
--i.e. parse = buildData . lexer

-- Lexer Splits a string into tokens
lexer :: String -> [String]
lexer = words

-- splitOnSemicolon :: [String] -> [[String]]
-- splitOnSemicolon = undefined -- Implemente a lógica para dividir a entrada em declarações

-- parseStm :: [String] -> Stm
-- parseStm = undefined -- Implemente a lógica para converter tokens em Stm


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
