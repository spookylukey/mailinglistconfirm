{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
import Ella.Framework
import Ella.Request (getPOST)
import Ella.Response
import Ella.Processors.General (addSlashRedirectView)

import Control.Exception (catchDyn)
import Database.HDBC (quickQuery, toSql, SqlError, commit)
import Database.HDBC.Sqlite3 (connectSqlite3)
import Maybe (isNothing)
import Random (randomRs, newStdGen)

-- Settings

sqlite_path = "/home/luke/httpd/lukeplant.me.uk/web/cgi-bin/data/addresses.db"
access_password = "mypassword"

-- Database

connect = connectSqlite3 sqlite_path

updateStatusStmnt = "UPDATE addresses SET send_email = ? WHERE id = ?;"
queryByIdStmnt  = "SELECT email FROM addresses WHERE id = ?;"
insertEntryStmnt = "INSERT INTO addresses (name, email, id) VALUES (?, ?, ?);"

update :: Bool -> String -> IO Bool
update addthem personid = do
  conn <- connect
  retval <- idpresent conn personid
  if retval
    then do
      quickQuery conn updateStatusStmnt [toSql addthem, toSql personid]
      commit conn
      return retval
    else do
      return retval

confirm = update True
remove = update False

idpresent conn personid = do
  vals <- quickQuery conn queryByIdStmnt [toSql personid]
  return (length vals == 1)

addEntry name email = do
  conn <- connect
  newid <- randomStr 10
  quickQuery conn insertEntryStmnt [toSql name, toSql email, toSql newid]
  commit conn

-- Error handling

sqlErrorHandler = \e -> do
                    let errMessage = show (e :: SqlError)
                    let resp = default500 errMessage
                    sendResponseCGI resp

-- Routing

views = [ addSlashRedirectView
        , "yes/" <+/> stringParam            //->  confirmEmailView  $ []
        , "no/" <+/> stringParam             //->  removeEmailView   $ []
        , "add/" <+/> empty                  //->  addEntryView      $ [passwordRequired]
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

-- -- Decorators

-- | Decorator that enforces a POST parameter 'password' to be present
-- and correct
passwordRequired :: View -> View
passwordRequired view req = do
  let password = getPOST "password" req
  case password of
    Nothing -> return ad
    Just pw | pw == access_password -> view req
            | otherwise -> return ad
 where ad = Just accessDenied

-- -- Clickable URLs

confirmEmailView personid req = do
  updated <- confirm personid
  return $ Just $ if (not updated)
                    then idNotFoundResponse
                    else addedResponse

removeEmailView personid req = do
  updated <- remove personid
  return $ Just $ if (not updated)
                    then idNotFoundResponse
                    else removedResponse

-- -- Admin URLs

addEntryView req = do
  let name  = getPOST "name" req
      email = getPOST "email" req
  if any isNothing [name, email]
     then return $ Just $ invalidInput "Please provide 'name' and 'email' parameters\n"
     else do
       addEntry name email
       return $ Just $ message "Added!\n"

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
