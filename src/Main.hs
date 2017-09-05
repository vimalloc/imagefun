module Main where

import Models.Category
import Queries.Category
import RestAPI.Category
import ConfigHelper

import Network.Wai.Handler.Warp (run)
import Data.Either.Utils
import Database.PostgreSQL.Simple
import Text.Printf

main :: IO ()
main = run 8081 app

{-
main :: IO ()
main = do
    cp <- getConfigParser
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
        False -> mapM_ print category

    -- TODO get better day then rows added back from this call
    -- TODO catch exception when unique constraint fails
    rowsAdded <- categoryInsert connection $ categoryToPG . makeCategory $ "Funny"
    putStrLn $ show rowsAdded
-}
