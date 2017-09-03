{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Category where

import Opaleye (Column, Table(Table), required, PGInt4, PGText, pgString, pgInt4)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)


data Category' id name = Category
    { categoryId   :: id
    , categoryName :: name
    } deriving (Show)
type Category       = Category' Int String
type CategoryColumn = Category' (Column PGInt4) (Column PGText)


$(makeAdaptorAndInstance "pCategory" ''Category')

categoryTable :: Table CategoryColumn CategoryColumn
categoryTable = Table "Categories" (pCategory Category
                                       { categoryId   = required "CategoryId"
                                       , categoryName = required "Name"
                                       })

categoryToPG :: Category -> CategoryColumn
categoryToPG = pCategory Category { categoryId   = pgInt4
                                  , categoryName = pgString
                                  }
