{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import HSQL.Database
import HSQL.SQL (runSelect)

main :: IO ()
main = do
  putStrLn "Creating an in-memory table and seeding rows..."
  let booksSchema = TableSchema
        { tableName = "books"
        , columns =
            [ ColumnDef "id" TInt
            , ColumnDef "title" TString
            , ColumnDef "author" TString
            ]
        , primaryKey = "id"
        }

  case createTable booksSchema >>= seedRows of
    Left err -> putStrLn $ "Failed to seed database: " <> T.unpack err
    Right table1 -> do
      putStrLn "All books:"
      renderRows (selectAll table1)

      putStrLn "SQL: SELECT id, title FROM books WHERE author = 'Ada'"
      case runSelect table1 "SELECT id, title FROM books WHERE author = 'Ada'" of
        Left err -> putStrLn $ "SQL query failed: " <> T.unpack err
        Right sqlRows -> renderRows sqlRows

      putStrLn "Books by Ada:" 
      renderRows (selectWhere (matchAuthor "Ada") table1)

      case updateWhere (matchAuthor "Ada") (addTag "FP Pioneer") table1 of
        Left err -> putStrLn $ "Update failed: " <> T.unpack err
        Right table2 -> do
          putStrLn "After tagging Ada's books:"
          renderRows (selectAll table2)

          let table3 = deleteWhere (matchTitle "Typed Lambda Calculi") table2
          putStrLn "After deleting a title:" 
          renderRows (selectAll table3)

seedRows :: Table -> Either Text Table
seedRows table0 = foldl step (Right table0) sampleRows
  where
    step acc row = acc >>= insertRow row

sampleRows :: [Row]
sampleRows =
  [ Map.fromList
      [ ("id", VInt 1)
      , ("title", VString "Lambda Calculus for Mortals")
      , ("author", VString "Ada")
      ]
  , Map.fromList
      [ ("id", VInt 2)
      , ("title", VString "Typed Lambda Calculi")
      , ("author", VString "Henk")
      ]
  , Map.fromList
      [ ("id", VInt 3)
      , ("title", VString "Functional Pearls")
      , ("author", VString "Ada")
      ]
  ]

matchAuthor :: Text -> Row -> Bool
matchAuthor target row = Map.lookup "author" row == Just (VString target)

matchTitle :: Text -> Row -> Bool
matchTitle target row = Map.lookup "title" row == Just (VString target)

addTag :: Text -> Row -> Row
addTag tag row = Map.insert "tag" (VString tag) row

renderRows :: [Row] -> IO ()
renderRows = mapM_ (putStrLn . formatRow)
  where
    formatRow row =
      let pairs = fmap renderPair (Map.toList row)
       in " - " <> T.unpack (T.intercalate ", " pairs)
    renderPair (k, v) = k <> "=" <> renderValue v
