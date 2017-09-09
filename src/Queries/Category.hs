{-# LANGUAGE Arrows #-}

module Queries.Category where

import           Models.Category

import           Data.Int (Int64)
import qualified Opaleye as O
import           Opaleye ((.==))
import           Control.Arrow (returnA)
import qualified Database.PostgreSQL.Simple as PGS


categoriesQuery :: O.Query CategoryColumnRead
categoriesQuery = O.queryTable categoryTable

categoryByIdQuery :: Int -> O.Query CategoryColumnRead
categoryByIdQuery id = proc () -> do
                           category   <- categoriesQuery -< ()
                           O.restrict -< categoryId category .== O.pgInt4 id
                           returnA    -< category

-- TODO remove this from queries as well.
categoryInsert :: PGS.Connection -> CategoryColumnWrite -> IO Int64
categoryInsert conn category = O.runInsertMany conn categoryTable [category]
