{-# LANGUAGE Arrows #-}

module Queries.Category where

import           Models.Category

import           Opaleye (queryTable, Query, runQuery, Column, PGInt4, PGText,
                          restrict, (.==), pgInt4)
import           Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS


categoriesQuery :: Query CategoryColumn
categoriesQuery = queryTable categoryTable

categoryByIdQuery :: Int -> Query CategoryColumn
categoryByIdQuery id = proc () -> do
                           category <- categoriesQuery -< ()
                           restrict -< categoryId category .== pgInt4 id
                           returnA  -< category

runCategoryQuery :: PGS.Connection -> Query CategoryColumn -> IO [Category]
runCategoryQuery = runQuery
