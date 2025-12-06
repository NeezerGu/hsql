{-# LANGUAGE OverloadedStrings #-}

module HSQL.SQL
  ( Operator(..)
  , ValueExpr(..)
  , Condition(..)
  , SelectList(..)
  , SelectQuery(..)
  , parseSelect
  , executeSelect
  , runSelect
  ) where

import Control.Monad (filterM, (>=>))
import Data.Bifunctor (first)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec hiding (spaces)
import Text.Parsec.Expr
import Text.Parsec.Text (Parser)

import HSQL.Database

-- | Supported comparison operators for WHERE clauses.
data Operator = OpEq | OpNeq | OpLt | OpLte | OpGt | OpGte
  deriving (Eq, Show)

-- | Values used in comparisons.
data ValueExpr = ColumnRef Text | Literal Value
  deriving (Eq, Show)

-- | Boolean conditions for WHERE clauses.
data Condition
  = Comparison ValueExpr Operator ValueExpr
  | And Condition Condition
  | Or Condition Condition
  | Not Condition
  deriving (Eq, Show)

-- | SELECT targets.
data SelectList = SelectAll | SelectColumns [Text]
  deriving (Eq, Show)

-- | A parsed SELECT statement.
data SelectQuery = SelectQuery
  { selectList :: SelectList
  , fromTable :: Text
  , whereClause :: Maybe Condition
  }
  deriving (Eq, Show)

-- | Parse a SQL SELECT statement with MySQL-like syntax.
parseSelect :: Text -> Either Text SelectQuery
parseSelect sql =
  first (T.pack . show) $ parse selectQuery "SQL" (T.unpack sql)

-- | Execute a parsed SELECT against a single in-memory table.
executeSelect :: Table -> SelectQuery -> Either Text [Row]
executeSelect table query = do
  let expected = T.toLower (tableName (schema table))
      actual = T.toLower (fromTable query)
  if actual /= expected
    then Left $ "Unknown table '" <> fromTable query <> "'."
    else do
      filtered <- filterM (rowMatches (whereClause query)) (selectAll table)
      mapM (projectRow (selectList query)) filtered

-- | Parse and execute a SELECT statement in one step.
runSelect :: Table -> Text -> Either Text [Row]
runSelect table = parseSelect >=> executeSelect table

rowMatches :: Maybe Condition -> Row -> Either Text Bool
rowMatches Nothing _ = Right True
rowMatches (Just cond) row = evalCondition cond row

projectRow :: SelectList -> Row -> Either Text Row
projectRow SelectAll row = Right row
projectRow (SelectColumns cols) row = Map.fromList <$> mapM pick cols
  where
    pick col =
      case Map.lookup col row of
        Just v -> Right (col, v)
        Nothing -> Left $ "Unknown column '" <> col <> "' in SELECT list."

-- | Evaluate a boolean condition against a row.
evalCondition :: Condition -> Row -> Either Text Bool
evalCondition cond row =
  case cond of
    Comparison l op r -> do
      lv <- evalValueExpr l
      rv <- evalValueExpr r
      compareValues op lv rv
    And l r -> do
      lv <- evalCondition l row
      if not lv
        then pure False
        else evalCondition r row
    Or l r -> do
      lv <- evalCondition l row
      if lv
        then pure True
        else evalCondition r row
    Not c -> not <$> evalCondition c row
  where
    evalValueExpr (Literal v) = Right v
    evalValueExpr (ColumnRef name) =
      case Map.lookup name row of
        Just v -> Right v
        Nothing -> Left $ "Unknown column '" <> name <> "' in WHERE clause."

compareValues :: Operator -> Value -> Value -> Either Text Bool
compareValues op l r =
  case (l, r) of
    (VInt a, VInt b) -> Right (cmp a b)
    (VString a, VString b) -> Right (cmp a b)
    _ -> Left "Type mismatch in comparison."
  where
    cmp a b = case op of
      OpEq -> a == b
      OpNeq -> a /= b
      OpLt -> a < b
      OpLte -> a <= b
      OpGt -> a > b
      OpGte -> a >= b

-- Parsing helpers
selectQuery :: Parser SelectQuery
selectQuery = do
  spaces
  keyword "select"
  lst <- selectListParser
  keyword "from"
  tbl <- identifier
  mWhere <- optionMaybe (keyword "where" *> condition)
  spaces
  eof
  pure $ SelectQuery lst tbl mWhere

selectListParser :: Parser SelectList
selectListParser =
  (symbol "*" >> pure SelectAll)
    <|> (SelectColumns <$> identifier `sepBy1` symbol ",")

condition :: Parser Condition
condition = buildExpressionParser table term
  where
    table =
      [ [Prefix (keyword "not" >> pure Not)]
      , [Infix (keyword "and" >> pure And) AssocRight]
      , [Infix (keyword "or" >> pure Or) AssocRight]
      ]
    term = parens condition <|> comparison

comparison :: Parser Condition
comparison = do
  lhs <- valueExpr
  op <- operator
  rhs <- valueExpr
  pure $ Comparison lhs op rhs

valueExpr :: Parser ValueExpr
valueExpr =
  (Literal <$> valueLiteral)
    <|> (ColumnRef <$> identifier)

valueLiteral :: Parser Value
valueLiteral = intLiteral <|> stringLiteral

intLiteral :: Parser Value
intLiteral = lexeme $ VInt . read <$> many1 digit

stringLiteral :: Parser Value
stringLiteral = lexeme $ VString . T.pack <$> (char '\'' *> many stringChar <* char '\'')
  where
    stringChar = escapedQuote <|> noneOf "'"
    escapedQuote = try (string "''" *> pure '\'')

operator :: Parser Operator
operator = choice
  [ symbol "=" >> pure OpEq
  , try (symbol "<>" >> pure OpNeq)
  , try (symbol "!=" >> pure OpNeq)
  , symbol "<=" >> pure OpLte
  , symbol ">=" >> pure OpGte
  , symbol "<" >> pure OpLt
  , symbol ">" >> pure OpGt
  ]

identifier :: Parser Text
identifier = lexeme (quotedIdentifier <|> unquotedIdentifier)
  where
    quotedIdentifier = T.pack <$> between (char '`') (char '`') (many1 (noneOf "`"))
    unquotedIdentifier = T.pack <$> ((:) <$> identStart <*> many identChar)
    identStart = letter <|> char '_'
    identChar = alphaNum <|> char '_' <|> char '$'

keyword :: String -> Parser ()
keyword kw = lexeme . try $ do
  _ <- caseInsensitiveString kw
  notFollowedBy alphaNum
  pure ()

symbol :: String -> Parser String
symbol s = lexeme (string s)

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

spaces :: Parser ()
spaces = skipMany (oneOf " \t\n\r")

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

caseInsensitiveString :: String -> Parser String
caseInsensitiveString = traverse charCI

charCI :: Char -> Parser Char
charCI c = char (toLower c) <|> char (toUpper c)
