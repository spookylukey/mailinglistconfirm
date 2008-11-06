import Control.Exception (catchDyn)
import Database.HDBC (quickQuery, toSql, SqlError, commit)
import Database.HDBC.Sqlite3 (connectSqlite3)
import System.Environment (getArgs)

-- Settings

sqlite_path = "/home/luke/httpd/lukeplant.me.uk/web/cgi-bin/data/addresses.db"

-- Database

connect = connectSqlite3 sqlite_path

insertStatement = "INSERT INTO addresses (name, email, id) VALUES (?, ?, ?);"

insert conn name email personid = do

  quickQuery conn insertStatement [toSql name, toSql email, toSql personid]

main = do
  alldata <- getContents
  let rows = lines alldata
  let vals = map (split (== '\t')) rows
--  mapM_ (putStrLn . show) pairs
  conn <- connect
  mapM_ (\x -> insert conn (x!!0) (x!!1) (x!!2)) vals
  commit conn

split :: (a -> Bool) -> [a] -> [[a]]
split pred l = case dropWhile pred l of
                 [] -> []
                 s' -> w : split pred s''
                     where (w, s'') = break pred s'
