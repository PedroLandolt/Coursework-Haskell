import Data.List (intercalate, sortBy)
import Data.Char (isSpace, isDigit, isAlphaNum)


------------------------------ Part 1: Assembler ------------------------------


-- Machine instructions
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show

-- Sequence of instructions
type Code = [Inst]



-- Possible values in the stack/state
data Values = IntV Integer | BoolV Bool
    deriving Show

-- Stack definition
type Stack = [Values]

-- State definition
type State = [(String, Values)]

-- Instantiate empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Instantiate empty state
createEmptyState :: State
createEmptyState = []

-- Converts the giben stack to a string, as an ordered list of values separated by commas without spaces
stack2Str :: Stack -> String
stack2Str [] = ""
stack2Str [x] = case x of
    IntV a -> show a
    BoolV a -> show a
stack2Str (x:xs) = case x of
    IntV a -> show a ++ "," ++ stack2Str xs
    BoolV a -> show a ++ "," ++ stack2Str xs

-- Converts the given state to a string, as a list of pairs variable=value, in alphabetical order, separated by commas without spaces
state2Str :: State -> String
state2Str state = intercalate "," $ map pairToStr sortedState
  where
    pairToStr (var, value) = var ++ "=" ++ valueToStr value
    valueToStr (IntV n) = show n
    valueToStr (BoolV b) = show b
    sortedState = sortBy (\(var1, _) (var2, _) -> var1 `compare` var2) state



-- Machine instructions definitions

-- Pushes a constant value onto the stack
push :: Values -> Stack -> Stack
push x xs = x:xs

-- Adds the two top integer values of the stack and then pushes the result
add :: Stack -> Stack
add (IntV x : IntV y : xs) = IntV(x + y):xs
add (BoolV _ : _ : _) = error "Run-time error"
add _ = error "Run-time error"

-- Multiplies the two top integer values of the stack and then pushes the result
mult :: Stack -> Stack
mult (IntV x : IntV y : xs) = IntV(x * y):xs
mult (BoolV _ : _ : _) = error "Run-time error"
mult _ = error "Run-time error"

-- Subtracts the topmost element of the stack with the secont topmost one and then pushes the result
sub :: Stack -> Stack
sub (IntV x : IntV y : xs) = IntV(x - y):xs
sub (BoolV _ : _ : _) = error "Run-time error"
sub _ = error "Run-time error"

-- Compares the top two values (integers or booleans) of the stack for equality and then pushes the result
eq :: Stack -> Stack
eq (IntV x : IntV y : xs) = BoolV(x == y):xs
eq (BoolV x : BoolV y : xs) = BoolV(x == y):xs
eq (BoolV _ : _ : _) = error "Run-time error"
eq _ = error "Run-time error"

-- Determines whether the topmost stack element is less or equal to the second topmost element and then pushes the result
le :: Stack -> Stack
le (IntV x : IntV y : xs) = BoolV(x <= y):xs
le (BoolV _ : _ : _) = error "Run-time error"
le _ = error "Run-time error"

-- Logical and operation on the top two values of the stack and then pushes the result
myAnd :: Stack -> Stack
myAnd (BoolV x : BoolV y : xs) = BoolV (x && y) : xs
myAnd _ = error "Run-time error"

-- Negates the topmost boolean value of the stack and then pushes the result
neg :: Stack -> Stack
neg (BoolV x : xs) = BoolV (not x) : xs
neg _ = error "Run-time error"

-- Pushes the value bound to the variable onto the stack
fetch :: String -> Stack -> State -> Stack
fetch variable stack (x:xs) | variable == fst x = push (snd x) stack
                            | otherwise = fetch variable stack xs
fetch variable stack [] = error "Run-time error"

-- Stores the topmost stack value into the variable and then pops the stack
store :: String -> Stack -> State -> State
store variable stack [] = [(variable, head stack)]
store variable stack (x:xs) | variable == fst x = (variable, head stack) : xs
                            | otherwise = x : store variable stack xs

-- Branches to the first code sequence if the topmost stack value is True, otherwise to the second code sequence
branch :: Code -> Code -> Stack -> State -> Code -> (Code, Stack, State)
branch c1 c2 stack state rest =
  case stack of
    (BoolV True : xs) -> run (c1 ++ rest, xs, state)
    (BoolV False : xs) -> run (c2 ++ rest, xs, state)
    _ -> error "Run-time error"

-- Loops the first code sequence while the topmost stack value is True, otherwise executes the second code sequence
loop :: Code -> Code -> Stack -> State -> Code -> (Code, Stack, State)
loop c1 c2 stack state rest =
    run (c1 ++ [Branch (c2 ++ [Loop c1 c2]) [Noop]] ++ rest, stack, state)



-- Interpreter that, given a list of instructions, a stack and a state, executes the instructions and returns the resulting stack and state
run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (x:xs, stack, state) =
    case x of
    Push a -> run (xs, push (IntV a) stack, state)
    Add -> run (xs, add stack, state)
    Mult -> run (xs, mult stack, state)
    Sub -> run (xs, sub stack, state)
    Equ -> run (xs, eq stack, state)
    Le -> run (xs, le stack, state)
    Tru -> run (xs, push (BoolV True) stack, state)
    Fals -> run (xs, push (BoolV False) stack, state)
    And -> run (xs, myAnd stack, state)
    Neg -> run (xs, neg stack, state)
    Fetch a -> run (xs, fetch a stack state, state)
    Store a -> run (xs, tail stack, store a stack state)
    Noop -> run (xs, stack, state)
    Branch c1 c2 -> branch c1 c2 stack state xs
    Loop c1 c2 -> loop c1 c2 stack state xs





------------------------------ Part 2: Compiler and Parser ------------------------------


-- Data structures to represent expressions and statements

-- Arithmetic expressions
data Aexp = Var String          -- Variable
          | IntConst Integer     -- Integer
          | AddP Aexp Aexp       -- Sum
          | SubtractP Aexp Aexp  -- Subtraction
          | MultiplyP Aexp Aexp  -- Multiplication
          deriving (Show)

-- Boolean expressions
data Bexp = TrueP Bool          -- Boolean True
          | FalseP Bool          -- Boolean False
          | LessEqP Aexp Aexp    -- Less Than or Equal
          | IntEqP Aexp Aexp     -- Integer Equality
          | BoolEqP Bexp Bexp    -- Boolean Equality
          | NotP Bexp            -- Boolean negation
          | AndP Bexp Bexp       -- Logical Operation
          deriving (Show)

-- Statements
data Stm = Assign String Aexp   -- Assignment
         | If Bexp [Stm] [Stm]   -- Conditional
         | While Bexp [Stm]      -- Loop
         deriving (Show)



-------- Compiler --------

-- Compiler function that, given a sequence of statements, returns the corresponding list of instructions
compile :: [Stm] -> Code
compile [] = []
compile (Assign var aexp : xs) = compA aexp ++ [Store var] ++ compile xs
compile (If bexp stm1 stm2 : xs) = compB bexp ++ [Branch (compile stm1) (compile stm2)] ++ compile xs
compile (While bexp stm : xs) = Loop (compB bexp) (compile stm) : compile xs

-- Compiler for arithmetic expressions
compA :: Aexp -> Code
compA (Var var) = [Fetch var]
compA (IntConst a) = [Push a]
compA (AddP aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Add]
compA (SubtractP aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (MultiplyP aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Mult]

-- Compiler for boolean expressions
compB :: Bexp -> Code
compB (TrueP bool) = if bool then [Tru] else [Fals]
compB (FalseP bool) = if bool then [Fals] else [Tru]
compB (LessEqP aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (IntEqP aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Equ]
compB (BoolEqP bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [Equ]
compB (NotP bexp) = compB bexp ++ [Neg]
compB (AndP bexp1 bexp2) = compB bexp2 ++ compB bexp1 ++ [And]



-------- Lexer --------

data Token = IdentToken String
           | ReservedToken String
           | ArithmeticOpToken String
           | IntConstToken Integer
           | VarToken String
           | BoolOpToken String
           | IntBoolOpToken String
           | SemiColonTok
           | OpenParenTok
           | CloseParenTok
           deriving (Show, Eq)

-- Lexer that, given an imperative program represented as a string, returns the corresponding list of tokens
lexer :: String -> [Token]
lexer input = concatMap lexerHelper (splitInput input)

-- Takes an imperative program represented by a string and splits it into into a list of individual tokens (which are later processed by the lexer), based on whitespace and semicolons.
splitInput :: String -> [String]
splitInput [] = []
splitInput (c:cs)
  | isAlphaNum c = takeWhile isAlphaNum (c:cs) : splitInput (dropWhile isAlphaNum cs)
  | isDigit c && ';' `elem` cs = [c, ';'] : splitInput (dropWhile (/= ';') cs)
  | isDigit c = takeWhile isDigit (c:cs) : splitInput (dropWhile isDigit cs)
  | isSpace c = splitInput (dropWhile isSpace cs)  
  | isBoolOp [c] || isArithmeticOp [c] = let (op, rest) = span (\x -> isBoolOp [x] || isArithmeticOp [x]) (c:cs) in op : splitInput rest
  | c == ':' && '=' `elem` cs = ":=" : splitInput (drop 2 cs)
  | c == '=' && '=' `elem` cs = "==" : splitInput (drop 2 cs)
  | c == '<' && '=' `elem` cs = "<=" : splitInput (drop 2 cs)
  | otherwise = [c] : splitInput cs

-- Helper function that, given a string, returns the corresponding list of tokens
lexerHelper :: String -> [Token]
lexerHelper word =
  case () of
    _ | isBoolOp word -> [BoolOpToken word]
      | isIntBoolOp word -> [IntBoolOpToken word]
      | isArithmeticOp word -> [ArithmeticOpToken word]
      | isReserved word -> [ReservedToken word]
      | isSemiColon word -> [SemiColonTok]
      | isOpenParen word -> [OpenParenTok]
      | isCloseParen word -> [CloseParenTok]
      | all isDigit (takeWhile (/=';') word) -> [IntConstToken (read (takeWhile (/=';') word))]
      | all isAlphaNum (take 1 word) -> [VarToken word]
      | otherwise -> error ("Cannot parse " ++ word ++ " on lexer")

-- Helper functions to check if a string is a token

-- Checks if a string is the boolean operator '='
isBoolOp :: String -> Bool
isBoolOp s = s == "="

-- Checks if a string is one of the boolean operators '==', '<='
isIntBoolOp :: String -> Bool
isIntBoolOp s = s `elem` ["==", "<="]

-- Checks if a string is one of the arithmetic operators ':=', '+', '-', '*'
isArithmeticOp :: String -> Bool
isArithmeticOp s = s `elem` [":=", "+", "-", "*"]

-- Checks if a string is one of the reserved words 'if', 'then', 'else', 'while', 'do', 'True', 'False', 'not', 'and', 'true', 'false'
isReserved :: String -> Bool
isReserved s = s `elem` ["if", "then", "else", "while", "do", "True", "False", "not", "and", "true", "false"]

-- Checks if a string is the semicolon ';'
isSemiColon :: String -> Bool
isSemiColon s = s == ";"

-- Checks if a string is the open parenthesis '('
isOpenParen :: String -> Bool
isOpenParen s = s == "("

-- Checks if a string is the close parenthesis ')'
isCloseParen :: String -> Bool
isCloseParen s = s == ")"



-------- Parser --------

-- Parser that, given an imperative program represented as a string, returns the corresponding list of statements
parse :: String -> [Stm]
parse = statementBuild . lexer

-- Given a list of tokens, returns the corresponding list of statements
statementBuild :: [Token] -> [Stm]
statementBuild [] = []
statementBuild (SemiColonTok:tokens) = statementBuild tokens

-- Assignments
statementBuild ((VarToken var):ArithmeticOpToken ":=":tokens) =
  Assign var (stmAexp aexp) : statementBuild rest
  where
    (aexp, rest) = break (== SemiColonTok) tokens

-- Conditional statements
statementBuild (ReservedToken "if":tokens) = If (stmBexp bexp) (statementBuild thenTokens) (statementBuild elseTokens) : statementBuild rest
    where (bexp, withThenTokens) = break (== ReservedToken "then") tokens
          afterThenTokens = tail withThenTokens
          (thenTokens, withElseTokens) =
                if head afterThenTokens == OpenParenTok then
                  getBetweenParenTokens afterThenTokens
                else
                    break (== SemiColonTok) afterThenTokens
          afterElseTokens =
                if head withElseTokens == SemiColonTok then
                  drop 2 withElseTokens   -- drop SemiColonTok and ReservedToken "else"
                else
                  tail withElseTokens     -- drop ReservedToken "else"
          (elseTokens, rest) =
                if head afterElseTokens == OpenParenTok then    -- if parenthesis
                    getBetweenParenTokens afterElseTokens       -- statements between parenthesis
                else
                    break (== SemiColonTok) afterElseTokens     -- only 1 statement w/o parenthesis

-- Loops
statementBuild (ReservedToken "while":tokens) = While (stmBexp bexp) (statementBuild doTokens) : statementBuild rest
    where (bexp, withDoTokens) = break (== ReservedToken "do") tokens
          (doTokens, rest) =
                if head (tail withDoTokens) == OpenParenTok then
                    getBetweenParenTokens (tail withDoTokens)
                else
                    break (== SemiColonTok) (tail withDoTokens)

statementBuild tokens = error "Invalid program on statementBuild"


-- Arithmetic expression parser
stmAexp :: [Token] -> Aexp
stmAexp tokens =
  case parseSumSub tokens of
    Just (aexp, []) -> aexp
    Just _ -> error "Invalid program on stmAexp"
    Nothing -> error "Invalid program on stmAexp"

-- Handles Sum and Subtractions (lowest precedence) 
parseSumSub :: [Token] -> Maybe (Aexp, [Token])
parseSumSub tokens =
  case parseMult tokens of
    Just (aexp1, ArithmeticOpToken "+":tokensLeft1) ->
      case parseSumSub tokensLeft1 of
        Just (aexp2, tokensLeft2) ->
          Just (AddP aexp1 aexp2, tokensLeft2)
        Nothing ->
          Nothing
    Just (aexp1, ArithmeticOpToken "-":tokensLeft1) ->
      case parseSumSub tokensLeft1 of
        Just (aexp2, tokensLeft2) ->
          Just (SubtractP aexp1 aexp2, tokensLeft2)
        Nothing ->
          Nothing
    result ->
      result

-- Handles Multiplications (medium precedence)
parseMult :: [Token] -> Maybe (Aexp, [Token])
parseMult tokens =
  case parseIntVarParens tokens of
    Just (aexp1, ArithmeticOpToken "*":tokensLeft1) ->
      case parseMult tokensLeft1 of
        Just (aexp2, tokensLeft2) -> Just (MultiplyP aexp1 aexp2, tokensLeft2)
        Nothing -> Nothing
    result -> result

-- Handles Integers, Variables and Parenthesis (highest precedence)
parseIntVarParens :: [Token] -> Maybe (Aexp, [Token])
parseIntVarParens (IntConstToken int : tokensLeft) = Just (IntConst int, tokensLeft)
parseIntVarParens (VarToken var : tokensLeft) = Just (Var var, tokensLeft)
parseIntVarParens (OpenParenTok:tokensLeft1) =
  case parseSumSub tokensLeft1 of
    Just (aexp, CloseParenTok:tokensLeft2) -> Just (aexp, tokensLeft2)
    Just _ -> Nothing
    Nothing -> Nothing
parseIntVarParens _ = Nothing


-- Boolean expression parser
stmBexp :: [Token] -> Bexp
stmBexp tokens =
  case parseAnd tokens of
    Just (bexp, []) -> bexp
    Just _ -> error "Invalid program on stmBexp"
    Nothing -> error "Invalid program on stmBexp"

-- Handles True and False (highest precedence)
parseTrueFalse :: [Token] -> Maybe (Bexp, [Token])
parseTrueFalse (ReservedToken "True":tokensLeft) = Just (TrueP True, tokensLeft)
parseTrueFalse (ReservedToken "False":tokensLeft) = Just (FalseP False, tokensLeft)
parseTrueFalse (OpenParenTok:tokensLeft1) =
  case parseAnd tokensLeft1 of
    Just (bexp, CloseParenTok:tokensLeft2) -> Just (bexp, tokensLeft2)
    _ -> Nothing
parseTrueFalse _ = Nothing

-- Handles Boolean Equality Operation (highest precedence)
parseBoolEq :: [Token] -> Maybe (Bexp, [Token])
parseBoolEq tokens =
  case parseNot tokens of
    Just (bexp1, BoolOpToken "=":tokensLeft1) ->
      case parseBoolEq tokensLeft1 of
        Just (bexp2, tokensLeft2) -> Just (BoolEqP bexp1 bexp2, tokensLeft2)
        Nothing -> Nothing
    result -> result

-- Handles Integer Equality Operation (highest precedence)
parseIntEq :: [Token] -> Maybe (Bexp, [Token])
parseIntEq tokens =
  case parseSumSub tokens of
    Just (aexp1, IntBoolOpToken "==":tokensLeft1) ->
      case parseSumSub tokensLeft1 of
        Just (aexp2, tokensLeft2) -> Just (IntEqP aexp1 aexp2, tokensLeft2)
        Nothing -> Nothing
    result -> parseLessEq tokens

-- Handles Not Logical Operation (highest precedence)
parseNot :: [Token] -> Maybe (Bexp, [Token])
parseNot (ReservedToken "not": tokensLeft) =
  case parseNot tokensLeft of
    Just (bexp, tokensLeft') -> Just (NotP bexp, tokensLeft')
    Nothing -> Nothing
parseNot tokens = parseIntEq tokens

-- Handles Less (highest precedence)
parseLessEq :: [Token] -> Maybe (Bexp, [Token])
parseLessEq tokens =
  case parseSumSub tokens of
    Just (aexp1, IntBoolOpToken "<=":tokensLeft1) ->
      case parseSumSub tokensLeft1 of
        Just (aexp2, tokensLeft2) -> Just (LessEqP aexp1 aexp2, tokensLeft2)
        Nothing -> Nothing
    result -> parseTrueFalse tokens

-- Handles And Logical Operation (lowest precedence)
parseAnd :: [Token] -> Maybe (Bexp, [Token])
parseAnd tokens =
  case parseBoolEq tokens of
    Just (bexp1, ReservedToken "and":tokensLeft1) ->
      case parseAnd tokensLeft1 of
        Just (bexp2, tokensLeft2) -> Just (AndP bexp1 bexp2, tokensLeft2)
        Nothing -> Nothing
    result -> result


-- Extracts tokens enclosed with parentheses, returning them and the remaining tokens after the closing parenthesis
getBetweenParenTokens :: [Token] -> ([Token], [Token])
getBetweenParenTokens tokens = (elseTokens, restTokens)
  where (restTokens, _, elseTokens) = getBetweenParenTokensAux tokens "" []

-- Auxiliary function that ensures the correct handling of nested parentheses using a "stack" (string)
getBetweenParenTokensAux :: [Token] -> String -> [Token] -> ([Token], String, [Token])
getBetweenParenTokensAux [] stk res = ([], "", reverse res)

-- If the current token is an opening parenthesis
getBetweenParenTokensAux (OpenParenTok:tokens) stk res =
    getBetweenParenTokensAux tokens ('(':stk) res

-- If the current token is a closing parenthesis
getBetweenParenTokensAux (CloseParenTok:tokens) (s:stk) res =
    getBetweenParenTokensAux tokens stk res

-- If the current token is not a parenthesis
getBetweenParenTokensAux (tok:tokens) stk res
    | null stk   = (tok:tokens, "", reverse res)
    | otherwise = getBetweenParenTokensAux tokens stk (tok:res)






------------------------------ Part 3: Testing ------------------------------


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
    where (_,stack,state) = run(code, createEmptyStack, createEmptyState)

testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(compile (parse programCode), createEmptyStack, createEmptyState)


main :: IO ()
main = do
  putStrLn ""
  putStrLn "Test Assembler"

  putStrLn "Teste 1: "
  print $ testAssembler [Push 10, Push 4, Push 3, Sub, Mult]
  print $ testAssembler [Push 10, Push 4, Push 3, Sub, Mult] == ("-10","")
  putStrLn ""

  putStrLn "Teste 2: "
  print $ testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"]
  print $ testAssembler [Fals, Push 3, Tru, Store "var", Store "a", Store "someVar"] == ("","a=3,someVar=False,var=True")
  putStrLn ""

  putStrLn "Teste 3: "
  print $ testAssembler [Fals, Store "var", Fetch "var"]
  print $ testAssembler [Fals, Store "var", Fetch "var"] == ("False","var=False")
  putStrLn ""

  putStrLn "Teste 4: "
  print $ testAssembler [Push (-20), Tru, Fals]
  print $ testAssembler [Push (-20), Tru, Fals] == ("False,True,-20","")
  putStrLn ""

  putStrLn "Teste 5: "
  print $ testAssembler [Push (-20), Tru, Tru, Neg]
  print $ testAssembler [Push (-20), Tru, Tru, Neg] == ("False,True,-20","")
  putStrLn ""

  putStrLn "Teste 6: "
  print $ testAssembler [Push (-20), Tru, Tru, Neg, Equ]
  print $ testAssembler [Push (-20), Tru, Tru, Neg, Equ] == ("False,-20","")
  putStrLn ""

  putStrLn "Teste 7: "
  print $ testAssembler [Push (-20), Push (-21), Le]
  print $ testAssembler [Push (-20), Push (-21), Le] == ("True","")
  putStrLn ""

  putStrLn "Teste 8: "
  print $ testAssembler [Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"]
  print $ testAssembler [Push 5, Store "x", Push 1, Fetch "x", Sub, Store "x"] == ("","x=4")
  putStrLn ""

  -- print $ ("Testando excecoes 1: ")
  -- print $ testAssembler [Push 1, Push 2, And]
  -- print $ ("Testando excecoes 2: ")
  -- print $ testAssembler [Tru, Tru, Store "y", Fetch "x", Tru]

  putStrLn "Teste 9: "
  print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]
  print $ testAssembler [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]] == ("","fact=3628800,i=1")
  putStrLn ""

  putStrLn "Test Parser"
  putStrLn ""

  putStrLn "Teste 1: "
  print $ testParser "x := 5; x := x - 1;"
  print $ testParser "x := 5; x := x - 1;" == ("","x=4")
  putStrLn ""

  putStrLn "Teste 2: "
  print $ testParser "x := 0 - 2;"
  print $ testParser "x := 0 - 2;" == ("","x=-2")
  putStrLn ""

  putStrLn "Teste 3: "
  print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
  print $ testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
  putStrLn ""

  putStrLn "Teste 4: "
  print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"
  print $ testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
  putStrLn ""

  putStrLn "Teste 5: "
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
  putStrLn ""

  putStrLn "Teste 6: "
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
  print $ testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
  putStrLn ""

  putStrLn "Teste 7: "
  print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"
  print $ testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
  putStrLn ""

  putStrLn "Teste 8: "
  print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;); else x := 1;"
  print $ testParser "x := 42; if x <= 43 then (x := 33; x := x+1;); else x := 1;" == ("","x=34")
  putStrLn ""

  putStrLn "Teste 9: "
  print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"
  print $ testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
  putStrLn ""

  putStrLn "Teste 10: "
  print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"
  print $ testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
  putStrLn ""

  putStrLn "Teste 11: "
  print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
  print $ testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
  putStrLn ""

  putStrLn "Teste 12: "
  print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
  print $ testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")
  putStrLn ""