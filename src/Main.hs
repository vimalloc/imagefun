module Main where

import           Models.Category
import           Queries.Category
import           Control.Monad
import           Data.ConfigFile
import           Data.Either.Utils
import           Database.PostgreSQL.Simple
import           Text.Printf
import qualified Data.ByteString.Char8 as BS

parseDbString :: ConfigParser -> Either CPError BS.ByteString
parseDbString cp = liftM5 buildDbString (getDb "host") (getDb "port")
                                        (getDb "username") (getDb "password")
                                        (getDb "dbname")
  where
    getDb = get cp "POSTGRES"

buildDbString :: String -> String -> String -> String -> String -> BS.ByteString
buildDbString host port user password dbname = BS.pack connStr
  where
    connTemplate = "host=%s port=%s user=%s password=%s dbname=%s"
    connStr      = printf connTemplate host port user password dbname


main :: IO ()
main = do
    val <- readfile emptyCP "imagefun.cfg"
    let cp = forceEither val

    let connStr = forceEither $ parseDbString cp
    connection <- connectPostgreSQL connStr

    categories <- runCategoryQuery connection categoriesQuery
    case null categories of
        True  -> putStrLn "No categories exist in the table"
        False -> mapM_ print categories

    let catId = 2
    category <- runCategoryQuery connection $ categoryByIdQuery catId
    case null category of
        True  -> putStrLn $ printf "Category id %d does not exist in the table" catId
        False -> mapM_ print categories
