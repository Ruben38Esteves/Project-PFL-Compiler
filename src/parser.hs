module Parser (Token(..), Aexp(..), Bexp(..), Stm(..), Program,
               lexer, parseIntOrParenthesis, parseAddOrProdOrInt,
               parseProdOrInt, parseSubOrAddOrProdOrInt, parseAexp,
               parseTrueOrFalseOrIntOrParenthesis, parseLeOrValue,
               parseIeqOrLeOrValue, parseNotOrIeqOrLeOrValue,
               parseEqOrNotOrIeqOrLeOrValue, parseAndOrEqOrNotOrLeOrIeqOrValue,
               parseBexp, parseStms, parseStm) where
import Data.Char
import Data.Type.Bool (If)
import Debug.Trace

-- Tokens
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

{- Translate string into tokens ------------------------------------------------------}
lexer :: String -> [Token]
lexer [] = []
lexer ('+' : restStr) = PlusTok : lexer restStr
lexer ('-' : restStr) = MinusTok : lexer restStr
lexer ('*' : restStr) = TimesTok : lexer restStr
lexer ('=' : '=' : restStr) = DEqTok : lexer restStr
lexer ('=' : restStr) = EqTok : lexer restStr
lexer (':' : '=' : restStr) = AssignTok : lexer restStr
lexer ('<' : '=' : restStr) = LeTok : lexer restStr
lexer ('(' : restStr) = OpenTok : lexer restStr
lexer (')' : restStr) = CloseTok : lexer restStr
lexer ('w' : 'h' : 'i' : 'l' : 'e' : restStr) = WhileTok : lexer restStr
lexer ('d' : 'o' : restStr) = DoTok : lexer restStr
lexer ('i' : 'f' : restStr) = IfTok : lexer restStr
lexer ('t' : 'h' : 'e' : 'n' : restStr) = ThenTok : lexer restStr
lexer ('e' : 'l' : 's' : 'e' : restStr) = ElseTok : lexer restStr
lexer ('a' : 'n' : 'd' : restStr) = AndTok : lexer restStr
lexer ('n' : 'o' : 't' : restStr) = NotTok : lexer restStr
lexer ('T' : 'r' : 'u' : 'e' : restStr) = TrueTok : lexer restStr
lexer ('F' : 'a' : 'l' : 's' : 'e' : restStr) = FalseTok : lexer restStr
lexer (';' : restStr) = SemiColonTok : lexer restStr
lexer (chr : restStr)
    | isSpace chr = lexer restStr
    | isAlpha chr = VarTok (takeWhile isAlpha (chr : restStr)) : lexer (dropWhile isAlpha restStr)
    | isDigit chr = let (number, rest) = span isDigit (chr : restStr) in IntTok (read number) : lexer rest
lexer (unexpectedChar : _) = error ("unexpected character: " ++ show unexpectedChar)

{- Functions to parse the list of Tokens and return the Arithmetic and Boolean expresions -----------------------------------}
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

