{-# LANGUAGE OverloadedStrings #-}

module HSQL.Database
  ( ColumnType (..)
  , ColumnDef (..)
  , TableSchema (..)
  , Value (..)
  , Row
  , Table
  , createTable
  , insertRow
  , selectAll
  , selectWhere
  , updateWhere
  , deleteWhere
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- | Supported primitive column types.
data ColumnType
  = TInt
  | TString
  deriving (Eq, Show)

-- | Definition of a column in a table schema.
data ColumnDef = ColumnDef
  { columnName :: Text
  , columnType :: ColumnType
  }
  deriving (Eq, Show)

-- | Schema for a table.
data TableSchema = TableSchema
  { tableName :: Text
  , columns :: [ColumnDef]
  , primaryKey :: Text
  }
  deriving (Eq, Show)

-- | Runtime values stored in rows.
data Value
  = VInt Int
  | VString Text
  deriving (Eq, Ord, Show)

type Row = Map Text Value

data Table = Table
  { schema :: TableSchema
  , rows :: Map Value Row
  }
  deriving (Eq, Show)

type Validation a = Either Text a

-- | Create an empty table from a schema, validating the primary key.
createTable :: TableSchema -> Validation Table
createTable s = do
  _ <- findPrimaryColumn s
  pure $ Table s Map.empty

findPrimaryColumn :: TableSchema -> Validation ColumnDef
findPrimaryColumn schemaDef =
  case filter ((== primaryKey schemaDef) . columnName) (columns schemaDef) of
    [col] -> Right col
    [] -> Left $ "Primary key column '" <> primaryKey schemaDef <> "' is not defined."
    _ -> Left "Primary key column appears multiple times."

-- | Insert a row into the table, validating types and primary-key uniqueness.
insertRow :: Row -> Table -> Validation Table
insertRow row table = do
  pkValue <- validateRowAgainstSchema (schema table) row
  if Map.member pkValue (rows table)
    then Left $ "Duplicate primary key value: " <> renderValue pkValue
    else pure $ table { rows = Map.insert pkValue row (rows table) }

-- | Return all rows ordered by primary key.
selectAll :: Table -> [Row]
selectAll = Map.elems . rows

-- | Filter rows by a predicate.
selectWhere :: (Row -> Bool) -> Table -> [Row]
selectWhere predicate = filter predicate . selectAll

-- | Update rows matching a predicate. The primary key is preserved and cannot change.
updateWhere :: (Row -> Bool) -> (Row -> Row) -> Table -> Validation Table
updateWhere predicate transform table = do
  let s = schema table
  updatedRows <- Map.traverseWithKey (applyTransform s) (rows table)
  pure $ table { rows = updatedRows }
  where
    applyTransform s pk row = do
      let newRow = if predicate row then enforcePrimaryKey row (transform row) else row
      pkValue <- validateRowAgainstSchema s newRow
      if pkValue == pk
        then Right newRow
        else Left $ "Primary key cannot be changed (expected " <> renderValue pk <> ", got " <> renderValue pkValue <> ")."

    pkName = primaryKey (schema table)

    enforcePrimaryKey oldRow newRow =
      case (Map.lookup pkName oldRow, Map.lookup pkName newRow) of
        (Just oldPk, Just newPk) | oldPk == newPk -> newRow
        (Just oldPk, _) -> Map.insert pkName oldPk newRow
        _ -> oldRow

-- | Delete rows matching a predicate.
deleteWhere :: (Row -> Bool) -> Table -> Table
deleteWhere predicate table =
  table { rows = Map.filter (not . predicate) (rows table) }

-- Validation helpers
validateRowAgainstSchema :: TableSchema -> Row -> Validation Value
validateRowAgainstSchema s row = do
  pkCol <- findPrimaryColumn s
  mapM_ (validateColumn row) (columns s)
  mapM_ (validateNoUnknownColumn s) (Map.keys row)
  case Map.lookup (columnName pkCol) row of
    Just pkValue -> pure pkValue
    Nothing -> Left $ "Primary key column '" <> columnName pkCol <> "' is missing."

validateColumn :: Row -> ColumnDef -> Validation ()
validateColumn row columnDef =
  case Map.lookup (columnName columnDef) row of
    Nothing -> Left $ "Column '" <> columnName columnDef <> "' is missing."
    Just v ->
      if valueMatchesType v (columnType columnDef)
        then Right ()
        else Left $ "Column '" <> columnName columnDef <> "' has wrong type."

validateNoUnknownColumn :: TableSchema -> Text -> Validation ()
validateNoUnknownColumn s colName =
  if any ((== colName) . columnName) (columns s)
    then Right ()
    else Left $ "Unknown column '" <> colName <> "' for table '" <> tableName s <> "'."

valueMatchesType :: Value -> ColumnType -> Bool
valueMatchesType value expectedType =
  case (value, expectedType) of
    (VInt _, TInt) -> True
    (VString _, TString) -> True
    _ -> False

renderValue :: Value -> Text
renderValue (VInt i) = T.pack (show i)
renderValue (VString t) = t
