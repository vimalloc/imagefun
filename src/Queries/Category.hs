{-# LANGUAGE Arrows #-}

module Queries.Category where

import           Models.Category


import           Data.Int
import           Opaleye (queryTable, Query, runQuery, Column, PGInt4, PGText,
                          restrict, (.==), pgInt4, pgString, runInsertMany)
import           Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS


categoriesQuery :: Query CategoryColumnRead
categoriesQuery = queryTable categoryTable

categoryByIdQuery :: Int -> Query CategoryColumnRead
categoryByIdQuery id = proc () -> do
                           category <- categoriesQuery -< ()
                           restrict -< categoryId category .== pgInt4 id
                           returnA  -< category

categoryInsert :: PGS.Connection -> CategoryColumnWrite -> IO Int64
categoryInsert conn category = runInsertMany conn categoryTable [category]

runCategoryQuery :: PGS.Connection -> Query CategoryColumnRead -> IO [CategoryRead]
runCategoryQuery = runQuery
