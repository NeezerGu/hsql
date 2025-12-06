# hsql

一个用 Haskell 编写的极简内存数据库示例，用于展示表定义、插入、查询、更新与删除等基本操作。示例可通过 `cabal run` 运行并在控制台打印结果。

## 功能

- 通过 `TableSchema` 定义表名、列和主键。
- 使用 `insertRow` 校验列类型、主键唯一性并写入数据。
- 支持 `selectAll` 与 `selectWhere` 进行查询。
- 通过 `HSQL.SQL.runSelect` 解析并执行 MySQL 风格的 `SELECT ... FROM ... WHERE ...` 语句（支持 `AND`/`OR`、括号、比较运算）。
- 通过 `updateWhere` 更新符合条件的行，同时保护主键不被篡改并持续做类型/字段校验。
- 使用 `deleteWhere` 删除符合条件的行。

## 运行示例

1. 安装 GHC 和 Cabal（若环境尚未安装）。
2. 在项目根目录执行：

   ```bash
   cabal run
   ```

   运行后会创建一个名为 `books` 的表，插入三本书籍，并展示查询、更新和删除的效果。

3. 示例也会执行一个 MySQL 风格的查询：

   ```sql
   SELECT id, title FROM books WHERE author = 'Ada'
   ```

   解析 SQL 字符串时支持 `AND`/`OR`、括号、`=`、`<>`/`!=`、`<`、`<=`、`>`、`>=` 等经典 MySQL 条件写法（不创造新语法）。
