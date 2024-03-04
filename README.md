# Project 2
>       Group: T11_G07

>   (50%) up202006479 - Ruben Silverio Fernandes Esteves

>   (50%) up202108663 - Artur Jose Albuquerque Oliveira

## Execution

The code can be executed ran under GHCi version 9.0
or later and by using the following commands:

```bash
#compile
ghc --make main.hs

#run
./main
```

## Project summary

This project is able to take a program in the form of a string, for example:
```
"i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"
```
And run it, returning the result:

```
("","fact=3628800,i=1")
```

the part 1 of the project implements the machine itself and the part 2 transforms the string into something the machine understands.

## Project description

### Part 2

The program starts by using the fucntion lexer to turn the input string into a list of tokens:

```haskell
lexer :: String -> [Token]
```

```haskell
data Token = PlusTok
    | MinusTok
    | TimesTok
    | OpenTok
    | CloseTok
    | IntTok Integer
    | EqTok
    | LeTok
    | VarTok String
    | IfTok
    | ThenTok
    | ElseTok
    | WhileTok
    | AndTok
    | NotTok
    | AssignTok
    | TrueTok
    | FalseTok
    | DEqTok
    | SemiColonTok
    | DoTok
    deriving (Show)
```
Tokens are used to better identify the components of the string and are then used by our parse functions to be turned into Aexp (Arithmetic expressions), Bexp (Boolean expressions) or Stm (Statements).

Inside our Aexp we put everything that is related to arithmetic from integers to variables and of course every operation available.

We followed the same idea for the Bexp separating the False and True for easier use when compiling and restricting the boolean operators to be associated with the values they needed, for example, Eq which is the "=" is restricted to only have Bexp as parameters since its job was to only compare booleans. We applied that same logic to every other operator.

Finally, for the Stm data we added the if and while which required both Bexp and a list of statements and the Assign that represents the assignement of Aexp, Integers or arithmetic operations, to a variable represented by the string.

```haskell
-- Arithmetic expressions
data Aexp =
  I Integer           -- constant
  | VarExp String        -- variables
  | AddExp Aexp Aexp     -- addition
  | MultExp Aexp Aexp    -- multiplication
  | SubExp Aexp Aexp    -- subtraction
  deriving Show

-- Boolean expressions
data Bexp =
     BTrue              -- true constant
    | BFalse           -- false constant
    | IEqExp Aexp Aexp    -- integer equality test
    | EqExp Bexp Bexp     -- equality test
    | LeExp Aexp Aexp     -- less than or equal to
    | NotExp Bexp         -- logical negation
    | AndExp Bexp Bexp    -- logical and
    deriving Show

-- Program as a list of statements
type Program = [Stm]

-- Statements
data Stm
  = Assign String Aexp    -- Assignment
  | If Bexp Program Program   -- If-then-else statement
  | While Bexp Program      -- While loop
  deriving Show
```

Our Parser was based on the one present in the lecture powerpoint, more precisely Aula Teórica 9 - Parsing em Haskell, which uses a parsing tchnique known as Recursive Descent Parsing. As seen earlier we started by converting the string into tokens removing whitespaces and just making the process of parsing easier overall.

We first started by the parseAexp since the parseAexp was going to be needed for the parseBexp and parseStms since the Data Bexp contained Aexp. The only difference between the parseAexp and the one from the powerpoint is the addition of the handling of parenthesis.

```haskell
{- Functions to parse the list of Tokens and return the Arithmetic expressions -----------------------------------}
parseIntOrParenthesis :: [Token] -> Maybe (Aexp, [Token])
parseIntOrParenthesis (IntTok n : restTokens) = Just (I n, restTokens)
parseIntOrParenthesis (VarTok var : restTokens) = Just (VarExp var, restTokens)
parseIntOrParenthesis (OpenTok : tokens) =
    case parseAexp tokens of
        Just (aexp, CloseTok : restTokens) -> Just (aexp, restTokens)
        _ -> Nothing
parseIntOrParenthesis _ = Nothing

parseAddOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseAddOrProdOrInt tokens =
    case parseProdOrInt tokens of
        Just (aexp1, PlusTok : restTokens) ->
            case parseAddOrProdOrInt restTokens of
                Just (aexp2, restTokens1) -> Just (AddExp aexp1 aexp2, restTokens1)
                _ -> Nothing
        result -> result

parseProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseProdOrInt tokens =
    case parseIntOrParenthesis tokens of
        Just (aexp1, TimesTok : restTokens) ->
            case parseProdOrInt restTokens of
                Just (aexp2, restTokens1) -> Just (MultExp aexp1 aexp2, restTokens1)
                _ -> Nothing
        result -> result

parseSubOrAddOrProdOrInt :: [Token] -> Maybe (Aexp, [Token])
parseSubOrAddOrProdOrInt tokens =
    case parseAddOrProdOrInt tokens of
        Just (aexp1, MinusTok : restTokens) ->
            case parseSubOrAddOrProdOrInt restTokens of
                Just (aexp2, restTokens1) -> Just (SubExp aexp1 aexp2, restTokens1)
                _ -> Nothing
        result -> result

parseAexp :: [Token] -> Maybe (Aexp, [Token])
parseAexp = parseSubOrAddOrProdOrInt
```

For our parseBexp we followed the same logic, we used the precedence given to us in the project specification, "The order of precedence for the operations is (with the first
one being executed first): integer inequality (≤), integer equality (==), logical
negation (not), boolean equality (=), logical conjunction (and)" and built our parsing function around it.

```haskell
parseTrueOrFalseOrIntOrParenthesis :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseTrueOrFalseOrIntOrParenthesis (TrueTok : restTokens) = Just (Right BTrue, restTokens)
parseTrueOrFalseOrIntOrParenthesis (FalseTok : restTokens) = Just (Right BFalse, restTokens)
parseTrueOrFalseOrIntOrParenthesis (IntTok n : restTokens) =
    case parseAexp (IntTok n : restTokens) of
        Just (aexp, restTokens1) -> Just (Left aexp, restTokens1)
        _ -> Nothing
parseTrueOrFalseOrIntOrParenthesis (VarTok var : restTokens) =
    case parseAexp (VarTok var : restTokens) of
        Just (aexp, restTokens1) -> Just (Left aexp, restTokens1)
        _ -> Nothing
parseTrueOrFalseOrIntOrParenthesis (OpenTok : tokens) =
    case parseBexp tokens of
        Just (bexp, CloseTok : restTokens) -> Just (Right bexp, restTokens)
        _ -> Nothing
parseTrueOrFalseOrIntOrParenthesis _ = Nothing

parseLeOrValue :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseLeOrValue tokens =
    case parseTrueOrFalseOrIntOrParenthesis tokens of
        Just (Left aexp1, LeTok : restTokens) ->
            case parseLeOrValue restTokens of
                Just (Left aexp2, restTokens1) -> Just (Right (LeExp aexp1 aexp2), restTokens1)
                _ -> Nothing
        result -> result

parseIeqOrLeOrValue :: [Token] -> Maybe (Either Aexp Bexp, [Token])
parseIeqOrLeOrValue tokens =
    case parseLeOrValue tokens of
        Just (Left aexp1, DEqTok : restTokens) ->
            case parseIeqOrLeOrValue restTokens of
                Just (Left aexp2, restTokens1) -> Just (Right (IEqExp aexp1 aexp2), restTokens1)
                _ -> Nothing
        result -> result

parseNotOrIeqOrLeOrValue :: [Token] -> Maybe (Bexp, [Token])
parseNotOrIeqOrLeOrValue (NotTok : restTokens) =
    case parseIeqOrLeOrValue restTokens of
        Just (Right bexp, restTokens1) -> Just (NotExp bexp, restTokens1)
        _ -> Nothing
parseNotOrIeqOrLeOrValue tokens =
    case parseIeqOrLeOrValue tokens of
        Just (Right bexp, restTokens) -> Just (bexp, restTokens)
        _ -> Nothing

parseEqOrNotOrIeqOrLeOrValue :: [Token] -> Maybe (Bexp, [Token])
parseEqOrNotOrIeqOrLeOrValue tokens =
    case parseNotOrIeqOrLeOrValue tokens of
        Just (bexp1, EqTok : restTokens) ->
            case parseEqOrNotOrIeqOrLeOrValue restTokens of
                Just (bexp2, restTokens1) -> Just (EqExp bexp1 bexp2, restTokens1)
                _ -> Nothing
        result -> result

parseAndOrEqOrNotOrLeOrIeqOrValue :: [Token] -> Maybe (Bexp, [Token])
parseAndOrEqOrNotOrLeOrIeqOrValue tokens =
    case parseEqOrNotOrIeqOrLeOrValue tokens of
        Just (bexp1, AndTok : restTokens) ->
            case parseAndOrEqOrNotOrLeOrIeqOrValue restTokens of
                Just (bexp2, restTokens1) -> Just (AndExp bexp1 bexp2, restTokens1)
                _ -> Nothing
        result -> result

parseBexp :: [Token] -> Maybe (Bexp, [Token])
parseBexp = parseAndOrEqOrNotOrLeOrIeqOrValue
```

And finally, the parseStms which, unlike the others, doesn't follow a logic of precedence since it wouldn't make sense.
The parseStms give is a list of Stm that can then be handled by our compiler.

```haskell
parseStms :: [Token] -> Maybe ([Stm], [Token])
parseStms tokens =
    case parseStm tokens of
        Just (stms1, restTokens) ->
            case restTokens of
                (SemiColonTok : restTokens1) ->
                    case parseStms restTokens1 of
                        Just (stms2, restTokens2) -> Just (stms1 ++ stms2, restTokens2)
                        _ -> Just (stms1, restTokens1)
                _ -> Just (stms1, restTokens)
        _ -> Nothing

parseNestedStms :: [Token] -> Maybe ([Stm], [Token])
parseNestedStms (OpenTok : restTokens) =
    case parseStms restTokens of
        Just (stms, CloseTok : SemiColonTok : restTokens1) -> Just (stms, restTokens1)
        _ -> Nothing
parseNestedStms tokens =
    case parseStm tokens of
        Just (stm, SemiColonTok : restTokens) -> Just (stm, restTokens)
        _ -> parseStms tokens

parseStm :: [Token] -> Maybe ([Stm], [Token])
parseStm (IfTok : restTokens) =
 case parseBexp restTokens of
  Just (bexp, ThenTok : restTokens1) ->
      case parseNestedStms restTokens1 of
          Just (stms1, elseTokens) ->
              case elseTokens of
                (ElseTok : restTokens2) ->
                    case parseNestedStms restTokens2 of
                        Just (stms2, restTokens3) ->
                            case parseStms restTokens3 of
                               Just (stms, restTokens4) -> Just (If bexp stms1 stms2 : stms, restTokens4)
                               _ -> Just ([If bexp stms1 stms2], restTokens3)
                        _ -> trace ("5, remaining tokens: " ++ show restTokens2) Nothing
                (IfTok : restTokens2) ->
                    -- Handle nested 'if' statements
                    case parseStm (IfTok : restTokens2) of
                        Just (stms, restTokens3) -> Just (stms, restTokens3)
                        _ -> trace ("4, remaining tokens: " ++ show restTokens2) Nothing
                _ -> trace ("3, remaining tokens: " ++ show elseTokens) Nothing
          _ -> trace ("2, remaining tokens: " ++ show restTokens1) Nothing
  _ -> trace ("1, remaining tokens: " ++ show restTokens) Nothing
parseStm (WhileTok : restTokens) =
 case parseBexp restTokens of
    Just (bexp, DoTok : restTokens1) ->
        case parseNestedStms restTokens1 of
            Just (stms, restTokens2) -> Just ([While bexp stms], restTokens2)
            _ -> Nothing
    _ -> Nothing
parseStm (VarTok var : AssignTok : restTokens) =
 case parseAexp restTokens of
    Just (aexp, restTokens1) -> Just ([Assign var aexp], restTokens1)
    _ -> Nothing
parseStm tokens = Nothing
```

And to wrap everything we have the parse function:

```
parse :: String -> Program
parse text = extractStm (parseStms (lexer text))
```

Our compiler functions (compile, compA and compB) are then responsible for turning those statements and expressions into Code:

```haskell
{- Functions to compile the program ------------------------------------------------}
compile :: Program -> Code
compile [] = []
compile (Assign var aexp : rest) = compA aexp ++ [Store var] ++ compile rest
compile (If bexp p1 p2 : rest) = compB bexp ++ [Branch (compile p1) (compile p2)] ++ compile rest
compile (While bexp p : rest) = Loop (compB bexp) (compile p) : compile rest

-- Compile arithmetic expressions
compA :: Aexp -> Code
compA (VarExp x) = [Fetch x]
compA (I n) = [Push n]
compA (AddExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Add]
compA (SubExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Sub]
compA (MultExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Mult]

-- Compile boolean expressions
compB :: Bexp -> Code
compB BTrue = [Tru]
compB BFalse = [Fals]
compB (IEqExp aexp1 aexp2) = compA aexp1 ++ compA aexp2 ++ [Equ]
compB (EqExp bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [Equ]
compB (LeExp aexp1 aexp2) = compA aexp2 ++ compA aexp1 ++ [Le]
compB (NotExp bexp) = compB bexp ++ [Neg]
compB (AndExp bexp1 bexp2) = compB bexp1 ++ compB bexp2 ++ [And]
```

### Part 1

Code is nothing more than a list of Instructions which are defined as follows:

```haskell
data Inst =
    Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
    Branch Code Code | Loop Code Code deriving Show

type Code = [Inst]
```

These instructions are then readable by our run function:

```haskell
run :: (Code, Stack, State) -> (Code, Stack, State)
```

This function takes the list of intructions and executes them on the values inside the **Stack**, storing values (when needed) in our **State**, which serves as storage.

State is defined as:

```haskell
type State = [(String, StackElem)]
```

**StackElem** is the type that is stored in our Stack data structure:

```haskell
data StackElem = IntElem Integer | BoolElem Bool deriving Show
data Stack = Stk [StackElem] deriving Show -- implementação usando listas

push :: StackElem -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> StackElem
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack
empty = Stk []

isEmpty :: Stack -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False

topInt :: Stack -> Integer
topInt stack = case top stack of
    IntElem value -> value
    _ -> error "Run-time error"

topBool :: Stack -> Bool
topBool stack = case top stack of
    BoolElem value -> value
    _ -> error "Run-time error"
```

## Conclusions

We feel like this was a great way to develop our skill in logical programming
and learn Haskell. It also made us understand all the work that happens behind the scenes in operations that we thought of as "simple".