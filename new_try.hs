-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 15/12/2023
-- Additional imports required for the code
import Data.List (intercalate, sortBy, deleteBy)
import Data.Function (on)
import Data.Ord (comparing)

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

-- Helper function to perform binary operations on the top two integers of the stack
performBinaryOp :: (Integer -> Integer -> Integer) -> Code -> Stack -> State -> (Code, Stack, State)
performBinaryOp op code stack state = case stack of
    (Left i1 : Left i2 : rest) -> run (code, Left (i1 `op` i2) : rest, state)
    _ -> error "Run-time error"

-- Helper function to perform comparison operations on the top two integers of the stack
performComparison :: (Integer -> Integer -> Bool) -> Code -> Stack -> State -> (Code, Stack, State)
performComparison cmp code stack state = case stack of
    (Left i1 : Left i2 : rest) -> run (code, Right (i1 `cmp` i2) : rest, state)
    _ -> error "Run-time error"

-- Helper function to perform binary boolean operations on the top two booleans of the stack
performBinaryBool :: (Bool -> Bool -> Bool) -> Code -> Stack -> State -> (Code, Stack, State)
performBinaryBool op code stack state = case stack of
    (Right b1 : Right b2 : rest) -> run (code, Right (b1 `op` b2) : rest, state)
    _ -> error "Run-time error"

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
  where
    performBinaryOp op (x:y:rest) = run (code, Left (op x y) : rest, state)
    performBinaryOp _ _ = error "Run-time error"
    
    performComparison cmp (x:y:rest) = run (code, Right (cmp x y) : rest, state)
    performComparison _ _ = error "Run-time error"
    
    performBinaryBool op (x:y:rest) = run (code, Right (op x y) : rest, state)
    performBinaryBool _ _ = error "Run-time error"

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
    -- where (_,stack,state) = run(code, createEmptyStack, createEmptyState)
    where (_, stack, state) = run (code, createEmptyStack, createEmptyState)

main :: IO ()
main = do

    -- Here, we'll just run a simple example program that uses some instructions.

    let (stackResult, stateResult) = testAssembler [Push 10, Push 4, Push 3, Sub, Mult]
    -- Examples:
    -- testAssembler [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
    -- testAssembler [Fals,Store "var",Fetch "var"] == ("False","var=False")
    -- testAssembler [Push (-20),Tru,Fals] == ("False,True,-20","")
    -- testAssembler [Push (-20),Tru,Tru,Neg] == ("False,True,-20","")
    -- testAssembler [Push (-20),Tru,Tru,Neg,Equ] == ("False,-20","")
    -- testAssembler [Push (-20),Push (-21), Le] == ("True","")
    -- testAssembler [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"] == ("","x=4")
    -- testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
    -- If you test:
    -- testAssembler [Push 1,Push 2,And]
    -- You should get an exception with the string: "Run-time error"
    -- If you test:
    -- testAssembler [Tru,Tru,Store "y", Fetch "x",Tru]
    -- You should get an exception with the string: "Run-time error"
    

    -- Run the program
    -- let (finalCode, finalStack, finalState) = run (program, initialStack, initialState)

    putStrLn $ "(\"" ++ stackResult ++ "\",\"" ++ stateResult ++ "\")"



