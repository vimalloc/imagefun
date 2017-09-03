{-# LANGUAGE Arrows #-}

module Queries.Category where

import           Models.Category

import           Opaleye (queryTable, Query, runQuery, Column, PGInt4, PGText)
import           Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS


categoriesQuery :: Query CategoryColumn
categoriesQuery = queryTable categoryTable

-- TODO move somewhere else. Generalize?
runCategoryQuery :: PGS.Connection -> Query CategoryColumn -> IO [Category]
runCategoryQuery = runQuery
