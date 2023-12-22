import Data.List
--import Stack
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
type Stack = [String] -- int or tt or ff
--type StackValue = Either Integer Bool
--type Stack = [StackValue]


--Define a new type to represent the machine’s state. 
--The type must be named State.
type State = [(String, Integer)]

createEmptyStack :: Stack
createEmptyStack =[]

--after executing the code [push−42, true, false], 
--the string representing the stack is: False,True,42.
stack2Str :: Stack -> String
stack2Str stack = intercalate "," (map show stack)

createEmptyState :: State
createEmptyState = []

--after executing the code 
-- [f alse, push − 3, true, store − var, store −a, store−someVar], 
--the string representing the state is: a=3,someVar=False,var=True
state2Str :: State -> String
state2Str state = intercalate "," [var ++ "=" ++ show val | (var, val) <- sortOn fst state]


run :: (Code, Stack, State) -> (Code, Stack, State)

run ([], stack, state) = ([], stack, state) -- No more instructions to execute
run (inst:restCode, stack, state) =
  case executeInstruction inst stack state of
    Left errorMessage -> error "Run-time error" 
    Right (newStack, newState) -> run (restCode, newStack, newState)
  
executeInstruction :: Inst -> Stack -> State -> Either String (Stack, State)
executeInstruction (Push n) stack state = Right (show n : stack, state)
-- executeInstruction (Fetch var) stack state =
--   case lookup var state of
--     Just val -> Right (show val : stack, state)
--     Nothing -> Left ("Variable not found: " ++ var)
-- executeInstruction (Store var) (x:rest) state =
--   case reads x of
--     [(val, "")] -> Right (rest, (var, val) : state)
--     _ -> Left "Invalid value on the stack for storage"
executeInstruction Add stack state =
  case stack of
    x:y:rest -> Right ( show ((read y + read x)) : rest, state)
    _ -> Left "Add: Insufficient operands on the stack"
executeInstruction _ stack state = Left "Unsupported operation" -- Add similar cases for other instructions

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

main :: IO ()
main = do
  let code = [Push 10, Push 5, Add]  -- Example code
--  let code = [Push 10, Push 5, Push (-3)]  -- Example code with only Push instructions

      (stackStr, stateStr) = testAssembler code
  putStrLn $ "Stack: " ++ stackStr
  putStrLn $ "State: " ++ stateStr

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