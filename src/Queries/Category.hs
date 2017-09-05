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

categoryInsert :: PGS.Connection -> CategoryColumnWrite -> IO Int64
categoryInsert conn category = O.runInsertMany conn categoryTable [category]

runCategoryQuery :: PGS.Connection -> O.Query CategoryColumnRead -> IO [CategoryRead]
runCategoryQuery = O.runQuery

-- TODO should raise error if more then one result is returned. Should never
--      happen as the id is a unique field in the db, but better safe then
--      sorry. Figure out how exceptions work.
-- TODO how would I write this without do notation?
categoryById :: PGS.Connection -> Int -> IO (Maybe CategoryRead)
categoryById conn id = do
    queryResult <- O.runQuery conn (categoryByIdQuery id)
    case queryResult of
        []   -> return Nothing
        x:[] -> return $ Just x
        x:xs -> return $ Just x -- TODO Raise error here
