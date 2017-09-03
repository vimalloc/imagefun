{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Categories (categoryQuery, runCategoryQuery) where

import           Prelude hiding (sum)

import           Opaleye (Column, Nullable, matchNullable, isNull,
                         Table(Table), required, queryTable,
                         Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                         (.===),
                         (.++), ifThenElse, pgString, aggregate, groupBy,
                         count, avg, sum, leftJoin, runQuery,
                         showSqlForPostgres, Unpackspec,
                         PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)

import           Data.Profunctor.Product (p2, p3)
import           Data.Profunctor.Product.Default (Default)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar (Day)

import           Control.Arrow (returnA)

import qualified Database.PostgreSQL.Simple as PGS



categoryTable :: Table (Column PGInt4, Column PGText)
                       (Column PGInt4, Column PGText)
categoryTable = Table "Categories" (p2 ( required "CategoryId"
                                       , required "Name" ))

categoryQuery :: Query (Column PGInt4, Column PGText)
categoryQuery = queryTable categoryTable


runCategoryQuery :: PGS.Connection
                 -> Query (Column PGInt4, Column PGText)
                 -> IO [(Int, String)]
runCategoryQuery = runQuery
