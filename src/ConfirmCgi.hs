{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
---
--- A simple CGI app designed to handle a personal mailing list
---
--- The public interface is simply 'yes' and 'no' links in emails.
--- Anyone invited to the list needs to be sent an email with
--- appropriate URLs for them to click.  These URLs are of the form
---
--- http://something.com/yes/<id>/
---
--- and
---
--- http://something.com/no/<id>/
---
--- where <id> is a random alphanumeric id stored in the DB.
---
--- The 'admin' interface consists of URLs that can be posted to (with
--- an appropriate password) to add/remove entries from the database,
--- or just confirm/remove those entries from being on the list.  It
--- is intended that this interface is accessed using curl or a
--- similar utility -- there is no HTML interface (yet).
---
--- For retrieving the list of 'yes' people, the only method currently
--- is to download the SQLite database.

import Ella.Framework
import Ella.Request (getPOST)
import Ella.Response
import Ella.Processors.General (addSlashRedirectView)

import Control.Exception (catchDyn, throwDyn)
import Database.HDBC (quickQuery, toSql, SqlError(SqlError), withTransaction)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Maybe (isNothing, fromJust)
import Random (randomRs, newStdGen)

-- Settings

-- Change these!
sqlite_path = "/home/luke/httpd/lukeplant.me.uk/web/cgi-bin/data/addresses.db"
access_password = "mypassword"

-- Database

connect = connectSqlite3 sqlite_path

updateStatusByIdStmnt = "UPDATE addresses SET send_email = ? WHERE id = ?;"
updateStatusByEmailStmnt = "UPDATE addresses SET send_email = ? WHERE email = ?;"
queryByIdStmnt  = "SELECT email FROM addresses WHERE id = ?;"
queryByEmailStmnt  = "SELECT email FROM addresses WHERE email = ?;"
insertEntryStmnt = "INSERT INTO addresses (name, email, id) VALUES (?, ?, ?);"
deleteEntryStmnt = "DELETE FROM addresses WHERE email = ?;"

updateById :: Bool -> String -> IO Bool
updateById addthem personid = do
  conn <- connect
  retval <- idpresent conn personid
  if retval
    then do
      withTransaction conn (\c -> quickQuery c updateStatusByIdStmnt [toSql addthem, toSql personid])
      return retval
    else do
      return retval

confirmById = updateById True
removeById = updateById False

idpresent conn personid = do
  vals <- quickQuery conn queryByIdStmnt [toSql personid]
  return (length vals == 1)

emailpresent conn email = do
  vals <- quickQuery conn queryByEmailStmnt [toSql email]
  return (length vals == 1)

addEntry :: String -> String -> IO ()
addEntry name email = do
  conn <- connect
  newid <- randomStr 10
  withTransaction conn (\c ->
                            quickQuery c insertEntryStmnt [toSql name, toSql email, toSql newid]
                       )
  return ()

deleteEntry :: String -> IO ()
deleteEntry email = do
  conn <- connect
  withTransaction conn (\c -> quickQuery c deleteEntryStmnt [toSql email])
  return ()

updateByEmail :: Bool -> String -> IO Bool
updateByEmail addthem email = do
  conn <- connect
  retval <- emailpresent conn email
  if retval
    then do
      withTransaction conn (\c -> quickQuery c updateStatusByEmailStmnt [toSql addthem, toSql email])
      return retval
    else do
      return retval

confirmByEmail = updateByEmail True
removeByEmail = updateByEmail False

-- Error handling

sqlErrorHandler = \e -> do
                    let errMessage = show (e :: SqlError)
                    let resp = default500 errMessage
                    sendResponseCGI resp

-- Routing

views = [ addSlashRedirectView
        -- Public
        , "yes/" <+/> stringParam            //->  confirmIdView     $ []
        , "no/" <+/> stringParam             //->  removeIdView      $ []
        -- Admin
        , "add/" <+/> empty                  //->  addEntryView      $ [passwordRequired]
        , "delete/" <+/> empty               //->  deleteEntryView   $ [passwordRequired]
        , "set/yes/" <+/> stringParam        //->  confirmEmailView  $ [passwordRequired]
        , "set/no/" <+/> stringParam         //->  removeEmailView   $ [passwordRequired]
        ]

-- Views

-- -- Utilities

message content = buildResponse [addContent content] utf8HtmlResponse

idNotFoundResponse = message "Sorry, the URL entered does not correspond to any known email address.  Please check you entered the full URL.\n"
addedResponse      = message "Thanks, I'll add you to my list.\n"
removedResponse    = message "Thanks, you won't be added you to my list.\n"

forbidden content = buildResponse [setStatus 403,
                                   addContent content] utf8HtmlResponse
accessDenied = forbidden "Access denied\n"

invalidInput content = buildResponse [ setStatus 400
                                     , addContent content] utf8HtmlResponse

emailNotFoundResponse = invalidInput "Email address not found.\n"

-- -- Decorators

-- | Decorator that enforces a POST parameter 'password' to be present
-- and correct
passwordRequired :: View -> View
passwordRequired view req = do
  let password = getPOST req "password"
  case password of
    Nothing -> return ad
    Just pw | pw == access_password -> view req
            | otherwise -> return ad
 where ad = Just accessDenied

-- -- Clickable URLs

confirmIdView personid req = do
  updated <- confirmById personid
  return $ Just $ if (not updated)
                    then idNotFoundResponse
                    else addedResponse

removeIdView personid req = do
  updated <- removeById personid
  return $ Just $ if (not updated)
                    then idNotFoundResponse
                    else removedResponse

-- -- Admin URLs

addEntryView req = do
  let name  = getPOST req "name"
      email = getPOST req "email"
  if any isNothing [name, email]
     then return $ Just $ invalidInput "Please provide 'name' and 'email' parameters\n"
     else do
       addEntry (fromJust name) (fromJust email)
       return $ Just $ message "Entry added to database\n"

deleteEntryView req = do
  let email = getPOST req "email"
  if isNothing email
     then return $ Just $ invalidInput "Please provide 'email' parameter"
     else do
       deleteEntry (fromJust email)
       return $ Just $ message "Entry removed from database\n"

confirmEmailView email req = do
  updated <- confirmByEmail email
  return $ Just $ if (not updated)
                    then emailNotFoundResponse
                    else message "Email added to mailing list.\n"

removeEmailView email req = do
  updated <- removeByEmail email
  return $ Just $ if (not updated)
                    then emailNotFoundResponse
                    else message "Email removed from mailing list.\n"

-- Utilities

randomStr :: Int -> IO String
randomStr n = do
    g <- newStdGen
    return $ take n (randomRs chars g)
  where chars = ('a','z')

-- Main

main :: IO ()
main = catchDyn (do
                  dispatchCGI views defaultDispatchOptions
                ) sqlErrorHandler
