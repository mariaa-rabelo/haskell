import Data.List
import qualified Stack as S

-- PFL 2023/24 - Haskell practical assignment quickstart

-- Part 1
-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

--Define a new type to represent the machine’s stack. 
--The type must be named Stack.
data StackValue = 
  IntVal Integer | 
  BoolVal Bool 
  deriving (Show)
type Stack = S.Stack StackValue

--Define a new type to represent the machine’s state. 
--The type must be named State.
type State = [(String, StackValue)]

createEmptyStack :: Stack
createEmptyStack = S.empty

--after executing the code [push−42, true, false], 
--the string representing the stack is: False,True,42.
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map showStackValue (S.stackToList stack))

showStackValue :: StackValue -> String
showStackValue (IntVal n) = show n
showStackValue (BoolVal b) = show b

createEmptyState :: State
createEmptyState = []

--after executing the code 
-- [f alse, push − 3, true, store − var, store −a, store−someVar], 
--the string representing the state is: a=3,someVar=False,var=True
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ showStackValue val | (var, val) <- sortOn fst state]

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state) -- No more instructions to execute
run (inst:restCode, stack, state) =
  case executeInstruction inst stack state of
    Left errorMessage -> error "Run-time error" 
    Right (newStack, newState) -> run (restCode, newStack, newState)
  
executeInstruction :: Inst -> Stack -> State -> Either String (Stack, State)
executeInstruction (Push n) stack state = Right (S.push (IntVal n) stack, state)
executeInstruction (Fetch var) stack state =
    case lookup var state of
        Just val -> Right (S.push val stack, state)
        Nothing -> Left ("Variable not found: " ++ var)
executeInstruction (Store var) stack state =
    if S.isEmpty stack
    then Left "Store: Stack is empty"
    else let strVal = S.top stack
             stack' = S.pop stack
          in case strVal of
              IntVal intValue -> Right (stack', (var, IntVal intValue) : state)
              BoolVal boolVal -> Right (stack', (var, BoolVal boolVal) : state)
        
-- executeInstruction Add stack state = performArithmetic (+) stack state
-- executeInstruction Mult stack state = performArithmetic (*) stack state
-- executeInstruction Sub stack state = performArithmetic (-) stack state
-- executeInstruction _ stack state = Left "Unsupported operation" -- Add similar cases for other instructions


-- executeInstruction Add stack state =
--   case stack of
--     x:y:rest -> Right ( show ((read y + read x)) : rest, state)
--     _ -> Left "Add: Insufficient operands on the stack"
-- executeInstruction _ stack state = Left "Unsupported operation" -- Add similar cases for other instructions

-- executeInstruction Mult stack state =
--   case stack of
--     x:y:rest -> Right ( show ((read y * read x)) : rest, state)
--     _ -> Left "Mult: Insufficient operands on the stack"
-- executeInstruction _ stack state = Left "Unsupported operation" -- Add similar cases for other instructions

-- Helper function for arithmetic operations
-- performArithmetic :: (Int -> Int -> Int) -> Stack -> State -> Either String (Stack, State)
-- performArithmetic op stack state =
--   case stack of
--     x:y:rest -> Right (show (x `op` y) : rest, state)
--     _ -> Left $ "Insufficient operands on the stack for " ++ show op

-- Execute arithmetic operations using the helper function
--executeInstruction :: Inst -> Stack -> State -> Either String (Stack, State)

-- add :: Stack -> Either String Stack
-- add stack =
--     if isEmpty stack || isEmpty (pop stack)
--     then Left "Add: Insufficient operands on the stack"
--     else Right $ push (Right (a + b)) rest
--     where
--         (Right a, tempStack) = (top stack, pop stack)
--         (Right b, rest) = (top tempStack, pop tempStack)


-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

main :: IO ()
main = do
  putStrLn $ "Test Push and Add: " ++ show testPushAndAdd
  putStrLn $ "Test Fetch and Store: " ++ show testFetchAndStore

testPushAndAdd :: Bool
testPushAndAdd =
  let (stackStr, _) = testAssembler [Push 10, Push 20, Add]
  in stackStr == "30"

testFetchAndStore :: Bool
testFetchAndStore =
  let (_, stateStr) = testAssembler [Push 15, Store "x", Fetch "x"]
  in stateStr == "x=15"
  
--  let code = [Push 10, Push 5, Add]  -- Example code
--  let code = [Push 10, Push 5, Push (-3)]  -- Example code with only Push instructions

  --     (stackStr, stateStr) = testAssembler code
  -- putStrLn $ "Stack: " ++ stackStr
  -- putStrLn $ "State: " ++ stateStr

-- Examples:
-- testAssembler [Push 10,Push 4,Push 3,Sub,Mult] == ("-10","")
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
-- You should get an exception with the string: "Run-time error

-- Part 2

-- TODO: Define the types Aexp, Bexp, Stm and Program

-- compA :: Aexp -> Code
compA = undefined -- TODO

-- compB :: Bexp -> Code
compB = undefined -- TODO

-- compile :: Program -> Code
compile = undefined -- TODO

-- parse :: String -> Program
parse = undefined -- TODO

--Hint: define an auxiliary function lexer :: String → [String] 
-- that splits the string into a list of words (tokens). 

--Example:
-- lexer ”23 + 4 * 421” = [”23”,”+”,”4”,”*”,”421”]
-- and from this list of tokens build the corresponding data, 
--i.e. parse = buildData . lexer


-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)

-- Examples:
-- testParser "x := 5; x := x - 1;" == ("","x=4")
-- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1 else y := 2" == ("","y=2")
-- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;)" == ("","x=1")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")