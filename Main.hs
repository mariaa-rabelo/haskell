-- PFL 2023/24 - Haskell practical assignment quickstart
-- Additional imports required for the code
import Data.List (intercalate, sortBy, deleteBy, isPrefixOf)
import Data.Function (on)
import Data.Ord (comparing)
import qualified Text.Parsec as Parsec
import Text.Parsec.String ( Parser )
import Data.Char (isSpace, isDigit, isLower, isAlpha)


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
    EqExpA Aexp Aexp |
    EqExpB Bexp Bexp |
    LeExp Aexp Aexp |
    AndExp Bexp Bexp |
    NotExp Bexp
    deriving Show

data Stm =
    Assign String Aexp |
    Seq [Stm] |
    If Bexp Stm Stm |
    While Bexp Stm
    deriving Show

type Program = [Stm]

compA :: Aexp -> Code
compA a = case a of
    (Const n) -> [Push n]
    (Var x) -> [Fetch x]
    (AddExp a1 a2) -> compA a1 ++ compA a2 ++ [Add]
    (SubExp a1 a2) -> compA a2 ++ compA a1 ++ [Sub]
    (MultExp a1 a2) -> compA a1 ++ compA a2 ++ [Mult]

compB :: Bexp -> Code
compB b = case b of
    TrueConst -> [Tru]
    FalseConst -> [Fals]
    (EqExpA a1 a2) -> compA a2 ++ compA a1 ++ [Equ]
    (EqExpB b1 b2) -> compB b2 ++ compB b1 ++ [Equ]
    (LeExp a1 a2) -> compA a2 ++ compA a1 ++ [Le]
    (AndExp b1 b2) -> compB b1 ++ compB b2 ++ [And]
    (NotExp b) -> compB b ++ [Neg]

compile :: Program -> Code
compile = concatMap compileStm
  where
    compileStm :: Stm -> Code
    compileStm (Assign x a) = compA a ++ [Store x]
    compileStm (Seq stms) = concatMap compileStm stms
    compileStm (If b s1 s2) = compB b ++ [Branch (compileStm s1) (compileStm s2)]
    compileStm (While b s) = [Loop (compB b ++ [Neg]) (compileStm s)]

-- Lexer Splits a string into tokens
lexer :: String -> [String]
lexer str =
    case str of
        [] -> []
        ('=':'=':cs) -> "==" : lexer cs
        ('<':'=':cs) -> "<=" : lexer cs
        (':':'=':cs) -> ":=" : lexer cs
        (c:cs)
            | isSpace c -> lexer cs
            | isDigit c -> let (numToken, restNum) = span isDigit (c:cs) in numToken : lexer restNum
            | isAlpha c -> let (varToken, restVar) = span isAlpha (c:cs) in varToken : lexer restVar
            | otherwise -> [c] : lexer cs

parse :: String -> [Stm]
parse str = parseStms (lexer str)

-- Parser for Aexp
parseAexp :: [String] -> (Aexp, [String])
parseAexp tokens =
    let (term, rest) = parseTerm tokens
    in case rest of
        ("+":rest') -> let (aexp, rest'') = parseAexp rest' in (AddExp term aexp, rest'')
        ("-":rest') -> let (aexp, rest'') = parseAexp rest' in (SubExp term aexp, rest'')
        _ -> (term, rest)

-- Parser for Aexp Terms
parseTerm :: [String] -> (Aexp, [String])
parseTerm tokens =
    let (factor, rest) = parseFactor tokens
    in case rest of
        ("*":rest') -> let (term, rest'') = parseTerm rest' in (MultExp factor term, rest'')
        _ -> (factor, rest)

-- Parser for Aexp Factors
parseFactor :: [String] -> (Aexp, [String])
parseFactor tokens =
    case tokens of
        "(" : rest ->
            case parseAexp rest of
                (aexp, ")":newRest) -> (aexp, newRest)
                _ -> error "parseFactor erro. close parentheses"
        t : rest
            | all isDigit t -> (Const (read t), rest)
            | otherwise -> (Var t, rest)
        _ -> error "parseFactor error."


parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens =
    let (term, rest) = parseEq tokens
    in case rest of
        ("and":rest') -> let (aexp, rest'') = parseBexp rest' in (AndExp term aexp, rest'')
        _ -> (term, rest)


parseEq :: [String] -> (Bexp, [String])
parseEq tokens =
    let (term, rest) = parseNot tokens
    in case rest of
        ("=":rest') -> let (aexp, rest'') = parseEq rest' in (EqExpB term aexp, rest'')
        _ -> (term, rest)

-- Parser for Boolean Expressions
parseNot :: [String] -> (Bexp, [String])
parseNot tokens =
    case tokens of
        ("not":rest) -> let (bexp, rest') = parseNot rest in (NotExp bexp, rest')
        rest -> let (bexp, rest') = parseRelation rest in (bexp, rest')


parseRelation :: [String] -> (Bexp, [String])

parseRelation ("(" : rest) =
    case parseBexp rest of
        (exp, ")":restTokens2) -> (exp, restTokens2)
        _ -> error "parse"

parseRelation tokens =
    case parseAexp tokens of
        (Var "True", rest) -> (TrueConst, rest)
        (Var "False", rest) -> (FalseConst, rest)
        _ -> let (a1, op:rest) = parseAexp tokens
                 (a2, rest') = parseAexp rest
             in case op of
                "<=" -> (LeExp a1 a2, rest')
                "==" -> (EqExpA a1 a2, rest')
                _ -> error "Parse error"


-- Parser for Statements
parseStm :: [String] -> (Stm, [String])
parseStm tokens =
    case tokens of
        ("if":rest) ->
            let (bexp, "then":restThen) = parseBexp rest

                (thenStm, "else":restElse) = if head restThen /= "("
                                               then parseStm restThen
                                             else
                                                let tokens_stms = takeWhile (/=")") (tail restThen)
                                                    stms = parseStms tokens_stms
                                                in (Seq stms, tail $ dropWhile (/= ")") restThen)

                (elseStm, rest'') = if head restElse /= "("
                                      then parseStm restElse
                                    else
                                        let tokens_stms = takeWhile (/=")") (tail restElse)
                                            stms = parseStms tokens_stms
                                        in (Seq stms, drop 2 $ dropWhile (/= ")") restElse)

            in (If bexp thenStm elseStm, rest'')

        ("while":rest) ->
            let (bexp, "do":restDo) = parseBexp rest


                (body, rest') = if head restDo /= "("
                                      then parseStm restDo
                                    else
                                        let tokens_stms = takeWhile (/=")") (tail restDo)
                                            stms = parseStms tokens_stms
                                        in (Seq stms, drop 2 $ dropWhile (/= ")") restDo)

            in (While bexp body, rest')

        (var:":=":rest) ->
            let (aexp,";":rest') = parseAexp rest
            in (Assign var aexp, rest')

        _ ->
            let (stm, rest') = parseSimpleStm tokens
            in case rest' of
                ";":rest'' -> let (stm', rest''') = parseStm rest'' in (Seq [stm, stm'], rest''')
                _ -> (stm, rest')


-- Parser for Simple Statements
parseSimpleStm :: [String] -> (Stm, [String])
parseSimpleStm tokens =
    case tokens of
        var:":=":rest ->
            let (aexp, rest') = parseAexp rest
            in case rest' of
                ";":rest'' -> (Assign var aexp, rest'')
                _ -> error "Parse error: Expected ';' after assignment"
        _ -> error "Parse error: Expected assignment statement"


-- Parsing a Seq of statements
parseStms :: [String] -> [Stm]
parseStms tokens =
    case tokens of
        [] -> []
        _ -> let (stm, rest) = parseStm tokens
             in stm : parseStms rest

-- To test the parser
testParser :: String -> (String, String)
testParser programCode =
    let (_, stack, state) = run(compile (parse programCode), createEmptyStack, createEmptyState)
    in (stack2Str stack, state2Str state)



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

    putStrLn "Testing parser:"
    -- print $ testParser "x := 5; x := x - 1;" == ("","x=4")
    -- print $ testParser "x := 0 - 2;" == ("","x=-2")
    -- print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
    -- print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
    -- print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
    -- print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
    -- print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
    -- print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
    -- print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
    -- print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
    -- print $ testParser "x := 2; y := (x - 3)(4 + 2*3); z := x +x(2);" == ("","x=2,y=-10,z=6")
    print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
