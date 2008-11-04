{-# OPTIONS_GHC -fglasgow-exts -XOverloadedStrings #-}
import Ella.Framework
import Ella.Response
import Ella.Processors.General (addSlashRedirectView)

import Control.Exception (catchDyn)
import Database.HDBC (quickQuery, toSql, SqlError, commit)
import Database.HDBC.Sqlite3 (connectSqlite3)

-- Settings

sqlite_path = "/home/luke/httpd/lukeplant.me.uk/web/cgi-bin/data/addresses.db"

-- Database

connect = connectSqlite3 sqlite_path

updateStatement = "UPDATE addresses SET send_email = ? WHERE id = ?;"
queryStatement  = "SELECT email FROM addresses WHERE id = ?;"

update :: Bool -> String -> IO Bool
update addthem personid = do
  conn <- connect
  retval <- idpresent conn personid
  if retval
    then do
      quickQuery conn updateStatement [toSql addthem, toSql personid]
      commit conn
      return retval
    else do
      return retval

confirm = update True
remove = update False

idpresent conn personid = do
  vals <- quickQuery conn queryStatement [toSql personid]
  return (length vals == 1)

-- Error handling

sqlErrorHandler = \e -> do
                    let errMessage = show (e :: SqlError)
                    let resp = default500 errMessage
                    sendResponseCGI resp

-- Routing

views = [ addSlashRedirectView
        , "yes/" <+/> stringParam            //->  addEmailView    $ []
        , "no/" <+/> stringParam             //->  removeEmailView $ []
        ]

-- Views

message content = buildResponse [addContent content] utf8HtmlResponse

idNotFoundResponse = message "Sorry, the URL entered does not correspond to any known email address.  Please check you entered the full URL."
addedResponse      = message "Thanks, I'll add you to my list."
removedResponse    = message "Thanks, you won't be added you to my list."

addEmailView personid req = do
  updated <- confirm personid
  return $ Just $ if (not updated)
                    then idNotFoundResponse
                    else addedResponse

removeEmailView personid req = do
  updated <- remove personid
  return $ Just $ if (not updated)
                    then idNotFoundResponse
                    else removedResponse

dispatchOptions = defaultDispatchOptions

main :: IO ()
main = catchDyn (do
                  dispatchCGI views dispatchOptions
                ) sqlErrorHandler
